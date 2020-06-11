module Apie.Types
  ( Hash
  , URL
  , Apie
  , ApieError(..)
  )
where

import Prelude

import Data.Maybe (Maybe)

type Hash = String
type URL = String

type Apie =
    { baseURL :: URL
    , username :: Maybe String
    , password :: Maybe String
    }

data ApieError
    = AuthenticationError
    | DeserialisationError String
    | DoesNotMatchExpected String
    | ResourceNotFound String
    | UnexpectedError String

instance showApieError :: Show ApieError where
  show = case _ of
    AuthenticationError -> "Authentication error"
    DeserialisationError x -> "Could not deserialize event:" <> x
    DoesNotMatchExpected x -> "Does not match expected version: " <> x
    ResourceNotFound x -> "Resource not found: " <> x
    UnexpectedError x -> "Unknown error: " <> x
