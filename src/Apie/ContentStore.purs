module Apie.ContentStore
  ( putFile
  )
where

import Prelude

import Affjax (Error(..), defaultRequest, request, printError)
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Apie.Auth (addAuthHeader)
import Apie.Types (ApieH, ApieError(..), Hash, URL)
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType (MediaType)
import Effect.Aff (Aff)


type PutResponse =
  { hash :: Hash
  , url :: URL
  }

putFile :: ApieH -> String -> MediaType -> RequestBody -> Aff (Either ApieError PutResponse)
putFile h filename mimeType body = do
    let req = defaultRequest
            { url = h.baseURL <> "/storage/" <> filename
            , method = Left PUT
            , responseFormat = RF.json
            , headers =
                [ ContentType mimeType ]
                # addAuthHeader h.username h.password
            , content = Just body
            , username = h.username
            , password = h.password
            , withCredentials = isJust h.username
            }
    res <- request req
    case res of
        Right resp -> do
            case A.decodeJson (resp.body) of
                Right pResp -> pure (Right pResp)
                Left err -> pure (Left (DeserialisationError err))
        Left (ResponseBodyError err _) -> pure (Left (DeserialisationError (show err)))
        Left err -> pure (Left (UnknownError (printError err)))
