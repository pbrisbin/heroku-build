module HerokuBuild.JSON
    ( Build(..)
    , Status(..)
    , SourceBlob(..)
    , Url
    , Version
    , newBuild
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson hiding (Success)
import Data.Text (Text)

data Build = Build
    { buildId :: Text
    , status :: Status
    , sourceBlob :: SourceBlob
    } deriving (Eq, Show)

data Status = Pending | Success | Unknown Text deriving (Eq, Show)

data SourceBlob = SourceBlob Url Version deriving (Eq, Show)

type Url = Text
type Version = Text

instance ToJSON Build where
    toJSON (Build _ _ (SourceBlob url version)) = object
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

newBuild :: Url -> Version -> Build
newBuild url version = Build undefined undefined $ SourceBlob url version
