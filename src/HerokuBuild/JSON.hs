{-# LANGUAGE OverloadedStrings #-}
module HerokuBuild.JSON
    ( Build(..)
    , Status(..)
    , SourceBlob(..)
    , Slug(..)
    , Url
    , Version
    , newBuild
    ) where

import Control.Monad
import Data.Aeson hiding (Success)
import Data.Text (Text)
import qualified Data.Text as T

data Build = Build
    { buildId :: Text
    , status :: Status
    , sourceBlob :: SourceBlob
    , slug :: Slug
    } deriving (Eq, Show)

data Status = Pending | Success | Unknown Text deriving (Eq)

instance Show Status where
    show Pending = "pending"
    show Success = "succeeded"
    show (Unknown t) = T.unpack t

data SourceBlob = SourceBlob Url Version deriving (Eq, Show)

data Slug = Slug (Maybe Text) deriving (Eq, Show)

type Url = Text
type Version = Text

instance ToJSON Build where
    toJSON (Build _ _ (SourceBlob url version) _) = object
        [ "source_blob" .= object
            [ "url" .= url
            , "version" .= version
            ]
        ]

instance FromJSON Build where
    parseJSON (Object o) = Build
        <$> o .: "id"
        <*> o .: "status"
        <*> o .: "source_blob"
        <*> o .: "slug"
    parseJSON _ = mzero

instance FromJSON SourceBlob where
    parseJSON (Object o) = SourceBlob
        <$> o .: "url"
        <*> o .: "version"
    parseJSON _ = mzero

instance FromJSON Status where
    parseJSON (String "pending") = pure Pending
    parseJSON (String "succeeded") = pure Success
    parseJSON (String s) = pure $ Unknown s
    parseJSON _ = mzero

instance FromJSON Slug where
    parseJSON (Object o) = Slug <$> o .:? "id"
    parseJSON _ = mzero

instance ToJSON Slug where
    toJSON (Slug sid) = object ["slug" .= sid]

newBuild :: Url -> Version -> Build
newBuild url version = Build undefined undefined (SourceBlob url version) undefined
