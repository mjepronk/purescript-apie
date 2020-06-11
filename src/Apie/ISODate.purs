module Apie.ISODate
    ( ISODate(..)
    , toString
    , fromString
    )
where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..))
import Data.Argonaut as A
import Data.Date (Date)
import Data.DateTime (DateTime(..), date)
import Data.Either (Either(..), fromRight, hush)
import Data.Enum (toEnum)
import Data.Formatter.DateTime (Formatter, unformatParser, parseFormatString, format)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Time (Time(..))
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser as P

newtype ISODate = ISODate Date

derive instance newtypeISODate :: Newtype ISODate _

instance encodeJsonISODate :: EncodeJson ISODate where
    encodeJson = A.fromString <<< toString

instance decodeJsonISODate :: DecodeJson ISODate where
    decodeJson json = do
        str <- A.decodeJson json
        fromString' str

toString :: ISODate -> String
toString = format isoDateFormat <<< flip DateTime midnight <<< unwrap

fromString :: String -> Maybe ISODate
fromString = hush <<< fromString'

fromString' :: String -> Either JsonDecodeError ISODate
fromString' str = do
    p <- P.runParserT str (unformatParser isoDateFormat)
    case p of
        Right dt -> Right (ISODate (date dt))
        Left err -> Left (UnexpectedValue (A.fromString str))

isoDateFormat ∷ Formatter
isoDateFormat = unsafePartial fromRight $ parseFormatString "YYYY-MM-DD"

midnight :: Time
midnight = unsafePartial $ Time
    (fromJust (toEnum 0))
    (fromJust (toEnum 0))
    (fromJust (toEnum 0))
    (fromJust (toEnum 0))
