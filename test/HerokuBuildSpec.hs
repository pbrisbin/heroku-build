module HerokuBuildSpec where

import HerokuBuild hiding (main)
import Test.Hspec

main :: IO ()
main = hspec spec

-- Do-nothing spec, just ensures all in-use modules are compiled for test
spec :: Spec
spec = return ()
