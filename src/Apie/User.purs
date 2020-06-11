module Apie.User where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Apie.Types (Apie, ApieError)
import Apie.Auth (addAuthHeader)
import Apie.Utils (parseResponse)
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff)

type UpdateResponse = { status :: String }

updateUser :: Apie -> Maybe String -> Maybe String -> Aff (Either ApieError UpdateResponse)
updateUser h password info = do
    let req = defaultRequest
            { url = h.baseURL <> "/user/update"
            , method = Left POST
            , responseFormat = RF.json
            , headers =
                [ ContentType applicationJSON ]
                # addAuthHeader h.username h.password
            , content = Just (RB.json (A.encodeJson { password, info }))
            , username = h.username
            , password = h.password
            , withCredentials = isJust h.username
            }
    resp <- request req
    pure (parseResponse resp)
