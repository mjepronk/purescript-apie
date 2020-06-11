module Apie.ISODateTime
    ( ISODateTime(..)
    )
where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..))
import Data.Argonaut as A
import Data.DateTime (DateTime)
import Data.Either (Either(..), fromRight)
import Data.Formatter.DateTime (Formatter, unformatParser, parseFormatString, format)
import Data.Newtype (class Newtype, unwrap)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser as P

newtype ISODateTime = ISODateTime DateTime

derive instance newtypeISODateTime :: Newtype ISODateTime _

instance encodeJsonISODateTime :: EncodeJson ISODateTime where
    encodeJson = A.fromString <<< format isoDateTimeFormat <<< unwrap

instance decodeJsonISODateTime :: DecodeJson ISODateTime where
    decodeJson json = do
        str <- A.decodeJson json
        p <- P.runParserT str (unformatParser isoDateTimeFormat)
        case p of
            Right dt -> Right (ISODateTime dt)
            Left _ -> Left (UnexpectedValue json)

isoDateTimeFormat ∷ Formatter
isoDateTimeFormat = unsafePartial fromRight $
    parseFormatString "YYYY-MM-DDTHH:mm:ss.SSSZ"
