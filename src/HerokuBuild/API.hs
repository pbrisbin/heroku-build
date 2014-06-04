module HerokuBuild.API
    ( ApiKey
    , postHeroku
    , getHeroku
    ) where

import Data.Aeson
import Data.Text (Text)

type ApiKey = Text

postHeroku :: ToJSON a => ApiKey -> Text -> a -> IO ()
postHeroku = undefined

getHeroku :: FromJSON a => ApiKey -> Text -> IO a
getHeroku = undefined
