module Apie.Diff
    ( diff
    , apply
    , getIdField
    )
where

import Prelude

import Data.Argonaut (Json, (.:), fromObject, toObject)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Foreign.Object (lookup, filterWithKey, union)

diff :: Json -> Json -> Either String Json
diff old new = do
  new' <- note "New is not an object" (toObject new)
  old' <- note "Old is not an object" (toObject old)
  pure <<< fromObject $ flip filterWithKey new' \key v ->
    let v' = lookup key old'
    in  key == "id" || v' /= Just v

apply :: Json -> Json -> Either String Json
apply old new = do
  new' <- note "New is not an object" (toObject new)
  old' <- note "Old is not an object" (toObject old)
  pure (fromObject (new' `union` old'))

getIdField :: Json -> Either String String
getIdField json = do
  obj <- note "Not an object" (toObject json)
  id <- obj .: "id"
  pure id
