module Apie.ContentStore
  ( putFile
  )
where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Apie.Auth (addAuthHeader)
import Apie.Types (Apie, ApieError, Hash, URL)
import Apie.Utils (parseResponse)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType (MediaType)
import Effect.Aff (Aff)


type PutResponse =
  { hash :: Hash
  , url :: URL
  }

putFile :: Apie -> String -> MediaType -> RequestBody -> Aff (Either ApieError PutResponse)
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
    resp <- request req
    pure (parseResponse resp)
