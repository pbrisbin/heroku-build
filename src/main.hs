module Main (main) where

import HerokuBuild
import qualified Data.Text as T

main :: IO ()
main = withOptions run

run :: Options -> IO ()
run (Options (Start url version)) = postHeroku "/builds" $ newBuild url version
run (Options (Status b)) = print . status =<< getHeroku ("/builds/" `T.append` b)
