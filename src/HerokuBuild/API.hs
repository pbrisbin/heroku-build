module HerokuBuild.API
    ( postHeroku
    , getHeroku
    ) where

import Data.Aeson
import Data.Text (Text)

postHeroku :: ToJSON a => Text -> a -> IO ()
postHeroku = undefined

getHeroku :: FromJSON a => Text -> IO a
getHeroku = undefined
