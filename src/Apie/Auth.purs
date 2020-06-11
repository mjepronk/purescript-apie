module Apie.Auth where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Apie.Types (Apie, ApieError)
import Apie.Utils (parseResponse)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType.Common (applicationJSON)
import Data.String.Base64 as B64
import Effect.Aff (Aff)

type User =
    { email :: String
    , info :: String
    }

type Auth = { user :: User }
type Info = { auth :: Auth }

getUserInfo :: Apie -> Aff (Either ApieError String)
getUserInfo h = do
    let req = defaultRequest
            { url = h.baseURL <> "/info"
            , method = Left GET
            , responseFormat = RF.json
            , headers =
                [ ContentType applicationJSON ]
                # addAuthHeader h.username h.password
            , username = h.username
            , password = h.password
            , withCredentials = isJust h.username
            }
    resp <- request req
    case parseResponse resp of
        Right (info :: Info) -> pure (Right info.auth.user.info)
        Left err -> pure (Left err)

addAuthHeader :: Maybe String -> Maybe String -> Array RequestHeader -> Array RequestHeader
addAuthHeader (Just user) (Just pass) hs = hs <> [RequestHeader "Authorization" header]
  where header = "Basic " <> (B64.encode (user <> ":" <> pass))
addAuthHeader _ _ hs = hs
