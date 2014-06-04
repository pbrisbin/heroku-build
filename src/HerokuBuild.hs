module HerokuBuild (module X) where

import qualified Data.Text as T

import HerokuBuild.API as X
import HerokuBuild.JSON as X
import HerokuBuild.Options as X

main :: IO ()
main = withOptions run

run :: Options -> IO ()
run (Options (Start url version)) = postHeroku "/builds" $ newBuild url version
run (Options (Status b)) = print . status =<< getHeroku ("/builds/" `T.append` b)
