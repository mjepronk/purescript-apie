module Apie.EventLog
  ( NewEvent(..)
  , Event(..)
  , appendEvent
  , getEvents
  , getEventsHead
  )
where

import Prelude

import Affjax (defaultRequest, printError, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader as RH
import Apie.Auth (addAuthHeader)
import Apie.ISODateTime (ISODateTime(..))
import Apie.Types (ApieError(..), Apie, Hash)
import Apie.UUID (UUID)
import Apie.UUID as UUID
import Apie.Utils (parseResponse)
import Data.Argonaut (Json, class EncodeJson, class DecodeJson, (.:), (:=), (~>))
import Data.Argonaut as A
import Data.Array (find)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Web.URL as URL
import Web.URL.URLSearchParams as USP


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


appendEvent :: Apie -> NewEvent -> Aff (Either ApieError Event)
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
    resp <- request req
    pure (parseResponse resp)

getEvents :: Apie -> Maybe Hash -> Maybe Hash -> Aff (Either ApieError (Array Event))
getEvents h fromHash toHash = do
    let req = defaultRequest
            { url = URL.href url
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
    pure (parseResponse resp)
  where
    url = URL.unsafeFromAbsolute (h.baseURL <> "/events")
          # (URL.setSearch <<< USP.toString
            $ USP.fromString ""
            # maybe identity (USP.append "from") fromHash
            # maybe identity (USP.append "to") toHash)


getEventsHead :: Apie -> Aff (Either ApieError (Maybe Hash))
getEventsHead h = do
    let req = defaultRequest
            { url = h.baseURL <> "/events"
            , method = Left HEAD
            , headers = addAuthHeader h.username h.password []
            , username = h.username
            , password = h.password
            , withCredentials = isJust h.username
            }
    resp <- request req
    case resp of
        Right r -> pure (Right (getHash r))
        Left err -> pure (Left (UnexpectedError (printError err)))
  where
    getHash resp = RH.value <$> (find (
        \hdr -> RH.name hdr == "etag" || RH.name hdr == "x-apie-hash") resp.headers)
