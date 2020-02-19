module Apie.Types
  ( Hash
  , URL
  , ApieH
  , ApieError(..)
  )
where

import Prelude

import Data.Maybe (Maybe)

type Hash = String
type URL = String

type ApieH =
    { baseURL :: URL
    , username :: Maybe String
    , password :: Maybe String
    }

data ApieError
    = DeserialisationError String
    | UnknownError String

instance showApieError :: Show ApieError where
  show = case _ of
    DeserialisationError x -> "Could not deserialize event:" <> x
    UnknownError x -> "Unknown error: " <> x
