{-# LANGUAGE OverloadedStrings #-}
module HerokuBuild.API
    ( ApiKey
    , getHeroku
    , postHeroku
    , postHeroku'
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as L

type ApiKey = ByteString

getHeroku :: FromJSON a => ApiKey -> String -> IO (Maybe a)
getHeroku k p = heroku k p $ \r -> r { method = "GET" }

postHeroku :: (ToJSON a, FromJSON b) => ApiKey -> String -> a -> IO (Maybe b)
postHeroku k p resource = heroku k p $ \r -> r
    { method = "POST", requestBody = RequestBodyLBS $ encode resource }

-- | Same as @'postHeroku'@ but discards the response body
postHeroku' :: ToJSON a => ApiKey -> String -> a -> IO ()
postHeroku' k p resource = heroku' k p $ \r -> r
    { method = "POST", requestBody = RequestBodyLBS $ encode resource }

heroku :: FromJSON a => ApiKey -> String -> (Request -> Request) -> IO (Maybe a)
heroku k p modify = fmap decode $ herokuReq k p modify

heroku' :: ApiKey -> String -> (Request -> Request) -> IO ()
heroku' k p modify = herokuReq k p modify >> return ()

herokuReq :: ApiKey -> String -> (Request -> Request) -> IO L.ByteString
herokuReq k p modify = do
    req <- parseUrlThrow $ herokuApi ++ p
    mgr <- newManager tlsManagerSettings
    rsp <- httpLbs (modify $ req { requestHeaders = herokuHeaders k }) mgr

    return $ responseBody rsp

herokuApi :: String
herokuApi = "https://api.heroku.com"

herokuHeaders :: ApiKey -> [Header]
herokuHeaders key =
    [ (hContentType, "application/json")
    , (hAccept, "application/vnd.heroku+json; version=3")
    , (hAuthorization, key)
    ]
