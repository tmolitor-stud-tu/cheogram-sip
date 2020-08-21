module Main (main) where

import Prelude ()
import BasicPrelude
import System.IO
	(stdout, stderr, hSetBuffering, BufferMode(LineBuffering))
import Control.Concurrent              (threadDelay)
import Control.Concurrent.STM          (STM)
import Data.Either                     (fromRight)
import Control.Error                   (exceptT, ExceptT(..), headZ, throwE, lastZ)
import Safe                            (maximumByMay)
import Control.Lens                    (over, set, at, _Right, traverseOf)
import Network                         (PortID (PortNumber))
import System.Clock                    (TimeSpec(..))
import Data.Time.Clock                 (getCurrentTime)
import Control.Monad.Loops             (anyM)
import qualified Focus
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Cache as Cache
import qualified Database.Redis as Redis
import qualified Network.Protocol.XMPP as XMPP
import qualified Network.Protocol.XMPP.Internal as XMPP
import qualified Data.XML.Types as XML

import qualified RedisURL
import Util

Just asteriskJid = XMPP.parseJID $ s"asterisk"

rewriteJingleInitiatorResponder iq
	| Just jingle <- child (s"{urn:xmpp:jingle:1}jingle") iq = iq {
			XMPP.iqPayload = Just $ jingle {
				XML.elementAttributes = map initiatorResponder (XML.elementAttributes jingle)
			}
		}
	| otherwise = iq
	where
	initiatorResponder (name, content)
		| name == s"initiator" = (name, [XML.ContentText $ maybe (s"") XMPP.formatJID (XMPP.iqFrom iq)])
		| name == s"responder" = (name, [XML.ContentText $ maybe (s"") XMPP.formatJID (XMPP.iqFrom iq)])
		| otherwise = (name, content)

bounceStanza (XMPP.ReceivedMessage m) from to =
	XMPP.putStanza $ m {
		XMPP.messageFrom = Just from,
		XMPP.messageTo = Just to
	}
bounceStanza (XMPP.ReceivedIQ iq) from to =
	XMPP.putStanza $ rewriteJingleInitiatorResponder $ iq {
		XMPP.iqFrom = Just from,
		XMPP.iqTo = Just to
	}
bounceStanza (XMPP.ReceivedPresence p) from to =
	XMPP.putStanza $ p {
		XMPP.presenceFrom = Just from,
		XMPP.presenceTo = Just to
	}

asteriskToReal :: XMPP.JID -> Maybe XMPP.JID -> Maybe (XMPP.JID, XMPP.JID)
asteriskToReal componentJid (Just XMPP.JID {
	XMPP.jidNode = Just escapedTo,
	XMPP.jidResource = Just escapedFrom
}) = (,) <$> XMPP.parseJID (unescapeJid $ XMPP.strNode escapedTo) <*>
	XMPP.parseJID (
		(escapeJid $ unescapeJid $ XMPP.strResource escapedFrom) ++ s"@" ++
		bareTxt componentJid ++ s"/sip"
	)
asteriskToReal _ _ = Nothing

realToAsterisk :: XMPP.JID -> Maybe XMPP.JID -> Maybe XMPP.JID -> Maybe XMPP.JID
realToAsterisk componentJid (Just from) (Just XMPP.JID {
	XMPP.jidNode = Just escapedTo
}) = XMPP.parseJID $
	(escapeJid $ bareTxt from) ++ s"@" ++
	bareTxt componentJid ++ s"/" ++
	(escapeJid $ unescapeJid $ XMPP.strNode escapedTo)
realToAsterisk _ _ _ = Nothing

receivedFrom (XMPP.ReceivedMessage s) = XMPP.stanzaFrom s
receivedFrom (XMPP.ReceivedPresence s) = XMPP.stanzaFrom s
receivedFrom (XMPP.ReceivedIQ s) = XMPP.stanzaFrom s

receivedTo (XMPP.ReceivedMessage s) = XMPP.stanzaTo s
receivedTo (XMPP.ReceivedPresence s) = XMPP.stanzaTo s
receivedTo (XMPP.ReceivedIQ s) = XMPP.stanzaTo s

jingleSid (XMPP.ReceivedIQ iq)
	| Just jingle <- child (s"{urn:xmpp:jingle:1}jingle") iq =
		XML.attributeText (s"sid") jingle
jingleSid _ = Nothing

sessionInitiateId (XMPP.ReceivedIQ iq)
	| Just jingle <- child (s"{urn:xmpp:jingle:1}jingle") iq,
	  XML.attributeText (s"action") jingle == Just (s"session-initiate") =
		((,) iq) <$> XML.attributeText (s"sid") jingle
sessionInitiateId _ = Nothing

main :: IO ()
main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering

	(componentJidTxt:host:portTxt:secret:redisURL:[]) <- getArgs
	let Just componentJid = XMPP.parseJID componentJidTxt
	let port = PortNumber $ read portTxt
	let server = XMPP.Server componentJid (textToString host) port
	let Right redisConnectInfo = RedisURL.parseConnectInfo $ textToString redisURL

	redis <- Redis.checkedConnect redisConnectInfo
	sessionInitiates <- Cache.newCache (Just $ TimeSpec 900 0)
	fullJids <- Cache.newCache (Just $ TimeSpec 900 0)
	-- exceptT print return $ runRoutedComponent server secret $ do

	Right () <- XMPP.runComponent server secret $ forever $ do
		stanza <- XMPP.getStanza
		case receivedFrom stanza of
			_ | XMPP.ReceivedIQ iq <- stanza,
			    XMPP.iqID iq == Just (s"CHEOGRAMIGNORE") -> return ()
			Just sfrom
				| sfrom == asteriskJid,
				  Just (iq, sid) <- sessionInitiateId stanza -> do
					let Just (to, from) = asteriskToReal componentJid $ receivedTo stanza
					liftIO $ Cache.purgeExpired sessionInitiates

					mostAvailable <- liftIO $ Redis.runRedis redis $ do
						Right resources <- Redis.hgetall (encodeUtf8 $ bareTxt to)
						jingleMessage <- anyM (fmap (fromRight False) . flip Redis.sismember (s"urn:xmpp:jingle-message:0")) $ map (B.drop 2 . snd) resources
						-- TODO: check if mostAvailable supports jingle audio. really we want most available that does
						return $ mfilter (const $ not jingleMessage) $
							(decodeUtf8 . fst <$> maximumByMay (comparing snd) resources)

					case mostAvailable of
						Just resource | Just fullToJid <- XMPP.parseJID (bareTxt to ++ s"/" ++ resource) -> do
							liftIO $ Cache.insert fullJids sid fullToJid
							bounceStanza (XMPP.ReceivedIQ iq) from fullToJid
						_ -> do
							liftIO $ Cache.insert sessionInitiates sid iq
							XMPP.putStanza $ (XMPP.emptyMessage XMPP.MessageNormal) {
									XMPP.messageID = Just $ s"proposal%" ++ sid,
									XMPP.messageTo = Just to,
									XMPP.messageFrom = Just from,
									XMPP.messagePayloads = [
										XML.Element (s"{urn:xmpp:jingle-message:0}propose")
											[(s"id", [XML.ContentText sid])]
											[XML.NodeElement $ XML.Element (s"{urn:xmpp:jingle:apps:rtp:1}description") [(s"media", [XML.ContentText $ s"audio"])] []]
									]
								}
			Just sfrom | sfrom == asteriskJid ->
				let
					Just (to, from) = asteriskToReal componentJid $ receivedTo stanza
					msid = jingleSid stanza
				in do
					fullTo <- liftIO $ maybe (return Nothing) (Cache.lookup' fullJids) msid
					liftIO $ forM_ msid $ \sid -> forM_ fullTo $ Cache.insert fullJids sid
					bounceStanza stanza from (fromMaybe to fullTo)
			sfrom
				| XMPP.ReceivedMessage m <- stanza,
				  Just from <- sfrom,
				  Just to <- XMPP.stanzaTo m,
				  Just proceed <- child (s"{urn:xmpp:jingle-message:0}proceed") m -> do
					let sid = fromMaybe mempty $ XML.attributeText (s"id") proceed
					minit <- liftIO $ Cache.lookup' sessionInitiates sid
					forM_ minit $ \init -> do
						liftIO $ Cache.delete sessionInitiates sid
						liftIO $ Cache.insert fullJids sid from
						bounceStanza (XMPP.ReceivedIQ init) to from
				| XMPP.ReceivedMessage m <- stanza,
				  Just from <- sfrom,
				  Just to <- XMPP.stanzaTo m,
				  Just reject <- child (s"{urn:xmpp:jingle-message:0}reject") m -> do
					let sid = fromMaybe mempty $ XML.attributeText (s"id") reject
					minit <- liftIO $ Cache.lookup' sessionInitiates sid
					forM_ minit $ \init -> do
						liftIO $ Cache.delete sessionInitiates sid
						XMPP.putStanza $ iqReply Nothing init
						XMPP.putStanza $ (XMPP.emptyIQ XMPP.IQSet) {
								XMPP.iqID = Just $ s"CHEOGRAMIGNORE",
								XMPP.iqTo = XMPP.iqFrom init,
								XMPP.iqFrom = XMPP.iqTo init,
								XMPP.iqPayload = Just $ XML.Element
									(s"{urn:xmpp:jingle:1}jingle")
									[
										(s"action", [XML.ContentText $ s"session-terminate"]),
										(s"sid", [XML.ContentText $ sid])
									]
									[XML.NodeElement $ XML.Element (s"{urn:xmpp:jingle:1}reason") [] [
										XML.NodeElement $ XML.Element (s"{urn:xmpp:jingle:1}decline") [] []
									]]
							}
				| XMPP.ReceivedMessage m <- stanza,
				  XMPP.messageType m == XMPP.MessageError,
				  Just errPayload <- lastZ $ XMPP.messagePayloads m,
				  Just sid <- T.stripPrefix (s"proposal%") =<< XMPP.messageID m -> do
					minit <- liftIO $ Cache.lookup' sessionInitiates sid
					forM_ minit $ \init -> do
						liftIO $ Cache.delete sessionInitiates sid
						XMPP.putStanza $ iqError errPayload init
				| Just from <- realToAsterisk componentJid sfrom (receivedTo stanza) ->
					bounceStanza stanza from asteriskJid
				| otherwise ->
					print ("DUNNO", stanza)

	return ()
