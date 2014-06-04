module HerokuBuild where

import System.Environment (getArgs)
import qualified Data.Text as T

import HerokuBuild.API
import HerokuBuild.JSON
import HerokuBuild.Options

main :: IO ()
main = withOptions run

-- TODO: further argument validation
run :: Options -> IO ()
run (Options Start) = do
    (url:version:_) <- getArgs
    postHeroku "/builds" $ newBuild (T.pack url) (T.pack version)

run (Options Status) = do
    (bid:_) <- getArgs
    print . status =<< getHeroku ("/builds/" `T.append` T.pack bid)
