module Main (main) where

import Data.Text (Text)
import Options.Applicative
import qualified Data.Text as T

import HerokuBuild

main :: IO ()
main = withOptions run

run :: Options -> IO ()
run (Options (Start url version)) = do
    k <- getApiKey
    postHeroku k "/builds" $ newBuild url version

run (Options (Status b)) = do
    k <- getApiKey
    print . status =<< getHeroku k ("/builds/" `T.append` b)

getApiKey :: IO Text
getApiKey = undefined
