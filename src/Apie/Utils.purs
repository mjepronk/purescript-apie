module Apie.Utils
  ( parseResponse )
where

import Prelude

import Affjax (Error(..), Response)
import Affjax.StatusCode (StatusCode(..))
import Apie.Types (ApieError(..))
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Either (Either(..))


parseResponse :: forall a. DecodeJson a => Either Error (Response Json) -> Either ApieError a
parseResponse x =
    case x of
        Right r -> do
            case r.status of
                StatusCode 200 ->
                    case decodeJson r.body of
                        Right r' -> Right r'
                        Left err -> Left (DeserialisationError err)
                _ -> Left (parseErrorMessage r)
        Left (RequestContentError err) -> Left (UnexpectedError ("Request content error: " <> show err))
        Left (ResponseBodyError err resp) -> Left (UnexpectedError ("Response body error: " <> show err))
        Left (XHRError err) -> Left (UnexpectedError ("XHR error: " <> show err))

parseErrorMessage :: Response Json -> ApieError
parseErrorMessage resp =
    case decodeJson resp.body of
        Right (r :: { errorMessage :: String }) ->
            case resp.status of
                StatusCode 400 -> DeserialisationError r.errorMessage
                StatusCode 401 -> AuthenticationError
                StatusCode 404 -> ResourceNotFound r.errorMessage
                StatusCode 412 -> DoesNotMatchExpected r.errorMessage
                StatusCode 500 -> UnexpectedError r.errorMessage
                StatusCode x -> UnexpectedError ("Unexpected HTTP status " <> show x)
        Left err -> DeserialisationError "Could not deserialize error response."
