module Apie.EventLog
  ( NewEvent(..)
  , Event(..)
  , appendEvent
  , getEvents
  , getEventsHead
  )
where

import Prelude

import Affjax (Error(..), defaultRequest, request, printError)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseHeader as RH
import Affjax.ResponseFormat as RF
import Apie.ISODateTime (ISODateTime(..))
import Apie.Types (ApieError(..), ApieH, Hash)
import Apie.Auth (addAuthHeader)
import Apie.UUID (UUID)
import Apie.UUID as UUID
import Data.Argonaut (Json, class EncodeJson, class DecodeJson, (.:), (:=), (~>))
import Data.Argonaut as A
import Data.Array (catMaybes, find)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import URI.Extra.QueryPairs as QP
import URI.Query as Q


newtype NewEvent = NewEvent
    { eventId   :: Maybe UUID
    , eventType :: String
    , body      :: Json
    }

instance showNewEvent :: Show NewEvent where
    show = A.stringify <<< A.encodeJson

instance encodeJsonNewEvent :: EncodeJson NewEvent where
    encodeJson (NewEvent x) =
        "eventId" := (UUID.toString <$> x.eventId)
        ~> "eventType" := x.eventType
        ~> "body" := x.body
        ~> A.jsonEmptyObject

newtype Event = Event
    { eventId   :: UUID
    , eventType :: String
    , body      :: Json
    , createdBy :: String
    , createdAt :: DateTime
    , hash      :: Hash
    }

instance showEvent :: Show Event where
    show = A.stringify <<< A.encodeJson

instance encodeJsonEvent :: EncodeJson Event where
    encodeJson (Event x) =
        "eventId" := UUID.toString x.eventId
        ~> "eventType" := x.eventType
        ~> "body" := x.body
        ~> "createdAt" := ISODateTime x.createdAt
        ~> "createdBy" := x.createdBy
        ~> "hash" := x.hash
        ~> A.jsonEmptyObject

instance decodeJsonEvent :: DecodeJson Event where
    decodeJson json = do
        x <- A.decodeJson json
        eventIdRaw <- x .: "eventId"
        eventType <- x .: "eventType"
        body <- x .: "body"
        createdAt <- unwrap <$> (x .: "createdAt" :: Either String ISODateTime)
        createdBy <- x .: "createdBy"
        hash <- x .: "hash"
        case UUID.parseUUID eventIdRaw of
            Just eventId -> do
                pure $ Event { eventId, eventType, body, createdAt, createdBy, hash }
            Nothing -> Left ("Invalid UUID")


appendEvent :: ApieH -> NewEvent -> Aff (Either ApieError Event)
appendEvent h newEvent = do
    let req = defaultRequest
            { url = h.baseURL <> "/events"
            , method = Left POST
            , responseFormat = RF.json
            , headers =
                [ ContentType applicationJSON ]
                # addAuthHeader h.username h.password
            , content = Just (RB.json (A.encodeJson newEvent))
            , username = h.username
            , password = h.password
            , withCredentials = isJust h.username
            }
    res <- request req
    case res of
        Right resp -> do
            case A.decodeJson (resp.body) of
                Right event -> pure (Right event)
                Left err -> pure (Left (DeserialisationError err))
        Left (ResponseBodyError err _) -> pure (Left (DeserialisationError (show err)))
        Left err -> pure (Left (UnknownError (printError err)))

getEvents :: ApieH -> Maybe Hash -> Maybe Hash -> Aff (Either ApieError (Array Event))
getEvents h fromHash toHash = do
    let req = defaultRequest
            { url = h.baseURL <> "/events" <> parameters
            , method = Left GET
            , responseFormat = RF.json
            , headers =
                [ ContentType applicationJSON ]
                # addAuthHeader h.username h.password
            , username = h.username
            , password = h.password
            , withCredentials = isJust h.username
            }
    res <- request req
    case res of
        Right resp -> do
            case A.decodeJson (resp.body) of
                Right event -> pure (Right event)
                Left err -> pure (Left (DeserialisationError err))
        Left (ResponseBodyError err _) -> pure (Left (DeserialisationError (show err)))
        Left err -> pure (Left (UnknownError (printError err)))
  where
    parameters = Q.print $ QP.print QP.keyFromString QP.valueFromString (QP.QueryPairs
        (catMaybes [(Tuple "from" <<< Just) <$> fromHash, (Tuple "to" <<< Just) <$> toHash]))

getEventsHead :: ApieH -> Aff (Either ApieError (Maybe Hash))
getEventsHead h = do
    let req = defaultRequest
            { url = h.baseURL <> "/events"
            , method = Left HEAD
            , headers = addAuthHeader h.username h.password []
            , username = h.username
            , password = h.password
            , withCredentials = isJust h.username
            }
    res <- request req
    case res of
        Right resp -> do
            pure (Right (getHash resp))
        Left err -> pure (Left (UnknownError (printError err)))
  where
    getHash resp = RH.value <$> (find (\hdr -> RH.name hdr == "etag") resp.headers)
