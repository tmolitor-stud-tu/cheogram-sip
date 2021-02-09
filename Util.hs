module Util where

import Prelude ()
import BasicPrelude
import Control.Concurrent.STM          (STM, atomically)
import Control.Applicative             (many)
import Control.Concurrent
	(ThreadId, forkFinally, myThreadId, throwTo)
import Data.Digest.Pure.SHA (sha1, bytestringDigest)
import Data.Void                       (absurd)
import Control.Error                   (exceptT)
import Data.Time.Clock                 (UTCTime)
import Data.Time.Format                (parseTimeM, defaultTimeLocale)
import qualified Control.Exception     as Ex
import qualified Data.Attoparsec.Text  as Atto
import qualified Data.Text             as Text
import qualified Data.XML.Types        as XML
import qualified Network.Protocol.XMPP as XMPP
import           UnexceptionalIO.Trans (Unexceptional (lift))
import qualified UnexceptionalIO.Trans as UIO
import qualified Data.ByteString.Lazy as LZ

instance Unexceptional XMPP.XMPP where
	lift = liftIO . UIO.run

s :: (IsString s) => String -> s
s = fromString

fromIO_ :: (Unexceptional m) => IO a -> m a
fromIO_ = exceptT absurd return . UIO.fromIO' (error . show)

atomicUIO :: (Unexceptional m) => STM a -> m a
atomicUIO = fromIO_ . atomically

escapeJid :: Text -> Text
escapeJid txt = mconcat result
	where
	Right result = Atto.parseOnly (many (
			slashEscape <|>
			replace ' ' "\\20" <|>
			replace '"' "\\22" <|>
			replace '&' "\\26" <|>
			replace '\'' "\\27" <|>
			replace '/' "\\2f" <|>
			replace ':' "\\3a" <|>
			replace '<' "\\3c" <|>
			replace '>' "\\3e" <|>
			replace '@' "\\40" <|>
			fmap Text.singleton Atto.anyChar
		) <* Atto.endOfInput) txt
	replace c str = Atto.char c *> pure (fromString str)

-- XEP-0106 says to only escape \ when absolutely necessary
slashEscape :: Atto.Parser Text
slashEscape =
	fmap (s"\\5c"++) $
	Atto.char '\\' *> Atto.choice escapes
	where
	escapes = map (Atto.string . fromString) [
			"20", "22", "26", "27", "2f", "3a", "3c", "3e", "40",
			"5c"
		]

unescapeJid :: Text -> Text
unescapeJid txt = fromString result
	where
	Right result = Atto.parseOnly (many (
			(Atto.char '\\' *> Atto.choice unescapes) <|>
			Atto.anyChar
		) <* Atto.endOfInput) txt
	unescapes = map (\(str, c) -> Atto.string (fromString str) *> pure c) [
			("20", ' '), ("22", '"'), ("26", '&'), ("27", '\''),
			("2f", '/'), ("3a", ':'), ("3c", '<'), ("3e", '>'),
			("40", '@'), ("5c", '\\')
		]

castException :: (Ex.Exception e1, Ex.Exception e2) => e1 -> Maybe e2
castException = Ex.fromException . Ex.toException

-- Re-throws all by ThreadKilled async to parent thread
-- Makes sync child exceptions async in parent, which is a bit sloppy
forkXMPP :: XMPP.XMPP () -> XMPP.XMPP ThreadId
forkXMPP kid = do
	parent <- liftIO myThreadId
	session <- XMPP.getSession
	liftIO $ forkFinally
		(void $ XMPP.runXMPP session kid)
		(either (handler parent) (const $ return ()))
	where
	handler parent e
		| Just Ex.ThreadKilled <- castException e = return ()
		| otherwise = throwTo parent e

iqReply :: Maybe XML.Element -> XMPP.IQ -> XMPP.IQ
iqReply payload iq = iq {
	XMPP.iqType = XMPP.IQResult,
	XMPP.iqFrom = XMPP.iqTo iq,
	XMPP.iqTo = XMPP.iqFrom iq,
	XMPP.iqPayload = payload
}

iqError :: XML.Element -> XMPP.IQ -> XMPP.IQ
iqError payload iq = (iqReply (Just payload) iq) {
	XMPP.iqType = XMPP.IQError
}

messageError :: XML.Element -> XMPP.Message -> XMPP.Message
messageError payload message = message {
	XMPP.messageType = XMPP.MessageError,
	XMPP.messageFrom = XMPP.messageTo message,
	XMPP.messageTo = XMPP.messageFrom message,
	XMPP.messagePayloads = payload : XMPP.messagePayloads message
}

notImplemented :: XML.Element
notImplemented =
	errorPayload "cancel" "feature-not-implemented" (s"Unknown request") []

child :: (XMPP.Stanza s) => XML.Name -> s -> Maybe XML.Element
child name = listToMaybe .
	(XML.isNamed name <=< XMPP.stanzaPayloads)

errorChild :: (XMPP.Stanza s) => s -> Maybe XML.Element
errorChild = child (s"{jabber:component:accept}error")

getBody :: (XMPP.Stanza s) => s -> Maybe Text
getBody = fmap (mconcat . XML.elementText) .
	child (s"{jabber:component:accept}body")

getSubject :: (XMPP.Stanza s) => s -> Maybe Text
getSubject = fmap (mconcat . XML.elementText) .
	child (s"{jabber:component:accept}subject")

errorPayload :: String -> String -> Text -> [XML.Node] -> XML.Element
errorPayload typ definedCondition english morePayload =
	XML.Element (s"{jabber:component:accept}error")
	[(s"type", [XML.ContentText $ fromString typ])]
	(
		XML.NodeElement (XML.Element definedConditionName [] []) :
		XML.NodeElement (XML.Element
				(s"{urn:ietf:params:xml:ns:xmpp-stanzas}text")
				[(s"xml:lang", [XML.ContentText $ s"en"])]
				[XML.NodeContent $ XML.ContentText english]
		) :
		morePayload
	)
	where
	definedConditionName = fromString $
		"{urn:ietf:params:xml:ns:xmpp-stanzas}" ++ definedCondition

bareJid :: XMPP.JID -> XMPP.JID
bareJid (XMPP.JID node domain _) = XMPP.JID node domain Nothing

bareTxt :: XMPP.JID -> Text
bareTxt (XMPP.JID (Just node) domain _) =
	mconcat [XMPP.strNode node, s"@", XMPP.strDomain domain]
bareTxt (XMPP.JID Nothing domain _) = XMPP.strDomain domain

parseXMPPTime :: Text -> Maybe UTCTime
parseXMPPTime =
	parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" . textToString

mkElement :: XML.Name -> Text -> XML.Element
mkElement name content = XML.Element name []
	[XML.NodeContent $ XML.ContentText content]

mkDiscoIdentity :: Text -> Text -> Text -> XML.Element
mkDiscoIdentity category typ name =
	XML.Element (s"{http://jabber.org/protocol/disco#info}identity") [
		(s"category", [XML.ContentText category]),
		(s"type", [XML.ContentText typ]),
		(s"name", [XML.ContentText name])
	] []

mkDiscoFeature :: Text -> XML.Element
mkDiscoFeature var =
	XML.Element (s"{http://jabber.org/protocol/disco#info}feature") [
		(s"var", [XML.ContentText var])
	] []

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

discoCapsIdentities :: XML.Element -> [Text]
discoCapsIdentities query =
	sort $
	map (\identity -> mconcat $ intersperse (s"/") [
		attrOrBlank (s"category") identity,
		attrOrBlank (s"type") identity,
		attrOrBlank (s"xml:lang") identity,
		attrOrBlank (s"name") identity
	]) $
	XML.isNamed (s"{http://jabber.org/protocol/disco#info}identity") =<<
		XML.elementChildren query

discoVars :: XML.Element -> [Text]
discoVars query =
	mapMaybe (XML.attributeText (s"var")) $
	XML.isNamed (s"{http://jabber.org/protocol/disco#info}feature") =<<
		XML.elementChildren query

data DiscoForm = DiscoForm Text [(Text, [Text])] deriving (Show, Ord, Eq)

oneDiscoForm :: XML.Element -> DiscoForm
oneDiscoForm form =
	DiscoForm form_type (filter ((/= s"FORM_TYPE") . fst) fields)
	where
	form_type = mconcat $ fromMaybe [] $ lookup (s"FORM_TYPE") fields
	fields = sort $ map (\field ->
			(
				attrOrBlank (s"var") field,
				sort (map (mconcat . XML.elementText) $ XML.isNamed (s"{jabber:x:data}value") =<< XML.elementChildren form)
			)
		) $
		XML.isNamed (s"{jabber:x:data}field") =<<
			XML.elementChildren form

discoForms :: XML.Element -> [DiscoForm]
discoForms query =
	sort $
	map oneDiscoForm $
	XML.isNamed (s"{jabber:x:data}x") =<<
		XML.elementChildren query

discoCapsForms :: XML.Element -> [Text]
discoCapsForms query =
	concatMap (\(DiscoForm form_type fields) ->
		form_type : concatMap (uncurry (:)) fields
	) (discoForms query)

discoToCaps :: XML.Element -> Text
discoToCaps query =
	mconcat (intersperse (s"<") (discoCapsIdentities query ++ discoVars query ++ discoCapsForms query)) ++ s"<"

discoToCapsHash :: XML.Element -> ByteString
discoToCapsHash query =
	LZ.toStrict $ bytestringDigest $ sha1 $ LZ.fromStrict $ encodeUtf8 $ discoToCaps query

attrOrBlank :: XML.Name -> XML.Element -> Text
attrOrBlank name el = fromMaybe mempty $ XML.attributeText name el
