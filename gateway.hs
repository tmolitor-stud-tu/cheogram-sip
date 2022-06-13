module Main (main) where

import Prelude ()
import BasicPrelude
import System.IO
	(stdout, stderr, hSetBuffering, BufferMode(LineBuffering))
import Data.Either                     (fromRight)
import Control.Error                   (lastZ)
import Safe                            (maximumByMay)
import System.Clock                    (TimeSpec(..))
import Control.Monad.Loops             (anyM)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Cache as Cache
import qualified Database.Redis as Redis
import qualified Network.Protocol.XMPP as XMPP
import qualified Data.XML.Types as XML

import qualified RedisURL
import Util

asteriskJid :: XMPP.JID
Just asteriskJid = XMPP.parseJID $ s"asterisk"

sipCapsHash :: Text
sipCapsHash = decodeUtf8 $ Base64.encode $ discoToCapsHash (sipDiscoInfo $ XML.Element (s"x") [] [])

sipAvailable :: XMPP.JID -> XMPP.JID -> XMPP.Presence
sipAvailable from to =
	(XMPP.emptyPresence XMPP.PresenceAvailable) {
		XMPP.presenceTo = Just to,
		XMPP.presenceFrom = XMPP.parseJID $ bareTxt from ++ s"/sip",
		XMPP.presencePayloads = [
			XML.Element (s"{http://jabber.org/protocol/caps}c") [
				(s"{http://jabber.org/protocol/caps}hash", [XML.ContentText $ s"sha-1"]),
				(s"{http://jabber.org/protocol/caps}node", [XML.ContentText $ s "xmpp:sip.cheogram.com"]),
				(s"{http://jabber.org/protocol/caps}ver", [XML.ContentText sipCapsHash])
			] []
		]
	}

sipDiscoFeatures :: [Text]
sipDiscoFeatures = [
		s"http://jabber.org/protocol/caps",
		s"http://jabber.org/protocol/disco#info",
		s"urn:xmpp:jingle-message:0",
		s"urn:xmpp:jingle:1",
		s"urn:xmpp:jingle:apps:dtls:0",
		s"urn:xmpp:jingle:apps:rtp:1",
		s"urn:xmpp:jingle:apps:rtp:audio",
		s"urn:xmpp:jingle:transports:ice-udp:1"
	]

sipDiscoInfo :: XML.Element -> XML.Element
sipDiscoInfo q = XML.Element (s"{http://jabber.org/protocol/disco#info}query")
			(map (\node -> (s"{http://jabber.org/protocol/disco#info}node", [XML.ContentText node])) $ maybeToList $ XML.attributeText (s"node") q) $
			XML.NodeElement (mkDiscoIdentity (s"client") (s"phone") (s"Cheogram SIP")) : map (XML.NodeElement . mkDiscoFeature) sipDiscoFeatures

gatewayDiscoFeatures :: [Text]
gatewayDiscoFeatures = [
		s"http://jabber.org/protocol/caps",
		s"http://jabber.org/protocol/disco#info",
		s"jid\\20escaping"
	]

gatewayCapsHash :: Text
gatewayCapsHash = decodeUtf8 $ Base64.encode $ discoToCapsHash (gatewayDiscoInfo $ XML.Element (s"x") [] [])

gatewayDiscoInfo :: XML.Element -> XML.Element
gatewayDiscoInfo q = XML.Element (s"{http://jabber.org/protocol/disco#info}query")
			(map (\node -> (s"{http://jabber.org/protocol/disco#info}node", [XML.ContentText node])) $ maybeToList $ XML.attributeText (s"node") q) $
			XML.NodeElement (mkDiscoIdentity (s"gateway") (s"sip") (s"Cheogram SIP")) : map (XML.NodeElement . mkDiscoFeature) gatewayDiscoFeatures

gatewayAvailable :: XMPP.JID -> XMPP.JID -> XMPP.Presence
gatewayAvailable from to =
	(XMPP.emptyPresence XMPP.PresenceAvailable) {
		XMPP.presenceTo = Just to,
		XMPP.presenceFrom = XMPP.parseJID $ bareTxt from ++ s"/gateway",
		XMPP.presencePayloads = [
			XML.Element (s"{http://jabber.org/protocol/caps}c") [
				(s"{http://jabber.org/protocol/caps}hash", [XML.ContentText $ s"sha-1"]),
				(s"{http://jabber.org/protocol/caps}node", [XML.ContentText $ s "xmpp:sip.cheogram.com"]),
				(s"{http://jabber.org/protocol/caps}ver", [XML.ContentText gatewayCapsHash])
			] []
		]
	}

rewriteJingleInitiatorResponder :: XMPP.IQ -> XMPP.IQ
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

bounceStanza :: XMPP.ReceivedStanza -> XMPP.JID -> XMPP.JID -> XMPP.XMPP ()
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
		escapeJid (unescapeJid $ XMPP.strResource escapedFrom) ++ s"@" ++
		bareTxt componentJid ++ s"/sip"
	)
asteriskToReal _ _ = Nothing

realToAsterisk :: XMPP.JID -> Maybe XMPP.JID -> Maybe XMPP.JID -> Maybe XMPP.JID
realToAsterisk componentJid (Just from) (Just XMPP.JID {
	XMPP.jidNode = Just escapedTo
}) = XMPP.parseJID $
	escapeJid (bareTxt from) ++ s"@" ++
	bareTxt componentJid ++ s"/" ++
	escapeJid (unescapeJid $ XMPP.strNode escapedTo)
realToAsterisk _ _ _ = Nothing

receivedFrom :: XMPP.ReceivedStanza -> Maybe XMPP.JID
receivedFrom (XMPP.ReceivedMessage stanza) = XMPP.stanzaFrom stanza
receivedFrom (XMPP.ReceivedPresence stanza) = XMPP.stanzaFrom stanza
receivedFrom (XMPP.ReceivedIQ stanza) = XMPP.stanzaFrom stanza

receivedTo :: XMPP.ReceivedStanza -> Maybe XMPP.JID
receivedTo (XMPP.ReceivedMessage stanza) = XMPP.stanzaTo stanza
receivedTo (XMPP.ReceivedPresence stanza) = XMPP.stanzaTo stanza
receivedTo (XMPP.ReceivedIQ stanza) = XMPP.stanzaTo stanza

jingleSid :: XMPP.ReceivedStanza -> Maybe Text
jingleSid (XMPP.ReceivedIQ iq)
	| Just jingle <- child (s"{urn:xmpp:jingle:1}jingle") iq =
		XML.attributeText (s"sid") jingle
jingleSid _ = Nothing

receivedIqId :: XMPP.ReceivedStanza -> Maybe Text
receivedIqId (XMPP.ReceivedIQ (XMPP.IQ { XMPP.iqID = Just iqID })) = Just iqID
receivedIqId _ = Nothing

sessionInitiateId :: XMPP.ReceivedStanza -> Maybe (XMPP.IQ, Text)
sessionInitiateId (XMPP.ReceivedIQ iq)
	| Just jingle <- child (s"{urn:xmpp:jingle:1}jingle") iq,
	  XML.attributeText (s"action") jingle == Just (s"session-initiate") =
		(,) iq <$> XML.attributeText (s"sid") jingle
sessionInitiateId _ = Nothing

-- Return the sessionID of a session-terminate, and Nothing if it's something else
-- (or the terminate doesn't have a session somehow)
sessionTerminateId :: XMPP.ReceivedStanza -> Maybe Text
sessionTerminateId (XMPP.ReceivedIQ iq)
	| Just jingle <- child (s"{urn:xmpp:jingle:1}jingle") iq,
	  XML.attributeText (s"action") jingle == Just (s"session-terminate") =
		XML.attributeText (s"sid") jingle
sessionTerminateId _ = Nothing

-- Decodes the JID and otherwise fowards the stanza on as-is
-- Also keeps the fullJidCache fresh
forwardOn :: XMPP.JID -> Cache.Cache Text XMPP.JID -> XMPP.ReceivedStanza -> XMPP.XMPP ()
forwardOn componentJid fullJidCache stanza = do
		fullTo <- liftIO $ maybe (return Nothing) (Cache.lookup' fullJidCache) msid
		liftIO $ forM_ msid $ \sid -> forM_ fullTo $ Cache.insert fullJidCache sid
		bounceStanza stanza from (fromMaybe to fullTo)
	where
	Just (to, from) = asteriskToReal componentJid $ receivedTo stanza
	msid = jingleSid stanza <|> receivedIqId stanza

main :: IO ()
main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering

	[componentJidTxt, host, portTxt, secret, redisURL] <- getArgs
	let Just componentJid = XMPP.parseJID componentJidTxt
	let port = read portTxt
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
						return $ mfilter (const $ not jingleMessage)
							(decodeUtf8 . fst <$> maximumByMay (comparing snd) resources)

					case mostAvailable of
						Just resource | Just fullToJid <- XMPP.parseJID (bareTxt to ++ s"/" ++ resource) -> do
							liftIO $ Cache.insert fullJids sid fullToJid
							bounceStanza (XMPP.ReceivedIQ iq) from fullToJid
						_ -> do
							liftIO $ Cache.insert sessionInitiates sid iq
							XMPP.putStanza $ (XMPP.emptyMessage XMPP.MessageChat) {
									XMPP.messageID = Just $ s"proposal%" ++ sid,
									XMPP.messageTo = Just to,
									XMPP.messageFrom = Just from,
									XMPP.messagePayloads = [
										XML.Element (s"{urn:xmpp:jingle-message:0}propose")
											[(s"id", [XML.ContentText sid])]
											[XML.NodeElement $ XML.Element (s"{urn:xmpp:jingle:apps:rtp:1}description") [(s"media", [XML.ContentText $ s"audio"])] []]
									]
								}
			Just sfrom
				| sfrom == asteriskJid,
				  Just (to, from) <- asteriskToReal componentJid $ receivedTo stanza,
				  Just sid <- sessionTerminateId stanza -> do
					mIq <- liftIO $ Cache.lookup' sessionInitiates sid
					case mIq of
						Just _ -> do
							liftIO $ Cache.delete sessionInitiates sid
							XMPP.putStanza $ (XMPP.emptyMessage XMPP.MessageChat) {
									XMPP.messageID = Just $ s"retract%" ++ sid,
									XMPP.messageTo = Just to,
									XMPP.messageFrom = Just from,
									XMPP.messagePayloads = [
										XML.Element (s"{urn:xmpp:jingle-message:0}retract")
											[(s"id", [XML.ContentText sid])]
											[]
									]
								}
						Nothing -> forwardOn componentJid fullJids stanza
			Just sfrom | sfrom == asteriskJid ->
				forwardOn componentJid fullJids stanza
			sfrom
				| XMPP.ReceivedPresence presence <- stanza,
				  Just from <- sfrom,
				  Just to <- XMPP.stanzaTo presence,
				  XMPP.PresenceSubscribe <- XMPP.presenceType presence -> do
					XMPP.putStanza $ (XMPP.emptyPresence XMPP.PresenceSubscribed) {
							XMPP.presenceTo = Just from,
							XMPP.presenceFrom = Just to
						}
					XMPP.putStanza $ (XMPP.emptyPresence XMPP.PresenceSubscribe) {
							XMPP.presenceTo = Just from,
							XMPP.presenceFrom = Just to
						}
					XMPP.putStanza $ case XMPP.jidNode to of
						Just _ -> sipAvailable to from
						Nothing -> gatewayAvailable to from
				| XMPP.ReceivedPresence presence <- stanza,
				  Just from <- sfrom,
				  Just to <- XMPP.stanzaTo presence,
				  XMPP.PresenceProbe <- XMPP.presenceType presence ->
					XMPP.putStanza $ case XMPP.jidNode to of
						Just _ -> sipAvailable to from
						Nothing -> gatewayAvailable to from
				| XMPP.ReceivedIQ iq <- stanza,
				  Just _ <- sfrom,
				  Just to <- XMPP.stanzaTo iq,
				  Just query <- child (s"{http://jabber.org/protocol/disco#info}query") iq ->
					XMPP.putStanza $ case XMPP.jidNode to of
						Just _ -> iqReply (Just $ sipDiscoInfo query) iq
						Nothing -> iqReply (Just $ gatewayDiscoInfo query) iq
				| XMPP.ReceivedMessage m <- stanza,
				  Just from <- sfrom,
				  Just to <- XMPP.stanzaTo m,
				  Just propose <- child (s"{urn:xmpp:jingle-message:0}propose") m -> do
					let sid = fromMaybe mempty $ XML.attributeText (s"id") propose
					liftIO $ Cache.insert fullJids sid from
					XMPP.putStanza $ (XMPP.emptyMessage XMPP.MessageNormal) {
							XMPP.messageID = Just $ s"proceed%" ++ sid,
							XMPP.messageTo = Just from,
							XMPP.messageFrom = XMPP.parseJID $ bareTxt to ++ s"/sip",
							XMPP.messagePayloads = [
								XML.Element (s"{urn:xmpp:jingle-message:0}proceed")
									[(s"id", [XML.ContentText sid])] []
							]
						}
					-- TODO: directed presence
				| XMPP.ReceivedMessage m <- stanza,
				  Just from <- sfrom,
				  Just to <- XMPP.stanzaTo m,
				  Just proceed <- child (s"{urn:xmpp:jingle-message:0}proceed") m -> do
					let sid = fromMaybe mempty $ XML.attributeText (s"id") proceed
					minit <- liftIO $ Cache.lookup' sessionInitiates sid
					forM_ minit $ \ini -> do
						liftIO $ Cache.delete sessionInitiates sid
						liftIO $ Cache.insert fullJids sid from
						bounceStanza (XMPP.ReceivedIQ ini) to from
				| XMPP.ReceivedMessage m <- stanza,
				  Just _ <- sfrom,
				  Just _ <- XMPP.stanzaTo m,
				  Just reject <- child (s"{urn:xmpp:jingle-message:0}reject") m -> do
					let sid = fromMaybe mempty $ XML.attributeText (s"id") reject
					minit <- liftIO $ Cache.lookup' sessionInitiates sid
					forM_ minit $ \ini -> do
						liftIO $ Cache.delete sessionInitiates sid
						XMPP.putStanza $ iqReply Nothing ini
						XMPP.putStanza $ (XMPP.emptyIQ XMPP.IQSet) {
								XMPP.iqID = Just $ s"CHEOGRAMIGNORE",
								XMPP.iqTo = XMPP.iqFrom ini,
								XMPP.iqFrom = XMPP.iqTo ini,
								XMPP.iqPayload = Just $ XML.Element
									(s"{urn:xmpp:jingle:1}jingle")
									[
										(s"action", [XML.ContentText $ s"session-terminate"]),
										(s"sid", [XML.ContentText sid])
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
					forM_ minit $ \ini -> do
						liftIO $ Cache.delete sessionInitiates sid
						XMPP.putStanza $ iqError errPayload ini
				| Just from <- realToAsterisk componentJid sfrom (receivedTo stanza) -> do
					liftIO $ forM_ sfrom $ \fullFrom -> forM_ (sessionInitiateId stanza) $ \(_, sid) ->
						Cache.insert fullJids sid fullFrom
					liftIO $ forM_ sfrom $ \fullFrom -> forM_ (receivedIqId stanza) $ \iqID ->
						Cache.insert fullJids iqID fullFrom
					bounceStanza stanza from asteriskJid
				| XMPP.ReceivedIQ iq <- stanza,
				  XMPP.iqType iq `elem` [XMPP.IQGet, XMPP.IQSet] ->
					XMPP.putStanza $ iqError notImplemented iq
				| otherwise ->
					print ("DUNNO", stanza)

	return ()
