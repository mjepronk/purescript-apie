module Apie.UUID
  ( UUID
  , randomUUID
  , parseUUID
  , toString
  )
where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..))
import Data.Argonaut as A
import Data.Array (elem)
import Data.Char.Unicode (digitToInt)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)

newtype UUID = UUID String

derive newtype instance eqUUID :: Eq UUID
derive newtype instance showUUID :: Show UUID
derive newtype instance encodeJsonUUID :: EncodeJson UUID

instance decodeJsonUUID :: DecodeJson UUID where
  decodeJson json = do
    str <- A.decodeJson json
    note (UnexpectedValue json) (parseUUID str)

foreign import randomUUID :: Effect UUID

parseUUID :: String -> Maybe UUID
parseUUID str = do
    _ <- match pattern str
    v <- extractVersion str
    case v of
        _ | v >= 1 && v <= 2 -> pure (UUID str)
          | v >= 3 && v <= 5 -> do
            -- For versions 3 and 4, they must specify a variant.
            variant <- extractVariant str
            if variant `elem` ['8', '9', 'a', 'b']
            then pure (UUID str)
            else Nothing
          | otherwise -> Nothing

toString :: UUID -> String
toString (UUID str) = str

extractVersion :: String -> Maybe Int
extractVersion str = charAt 14 str >>= digitToInt

extractVariant :: String -> Maybe Char
extractVariant = charAt 19

pattern :: Regex
pattern = unsafeRegex "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[0-9a-f]{4}-[0-9a-f]{12}$" ignoreCase
