module HerokuBuildSpec (main, spec) where

import HerokuBuild
import qualified Data.Text as T

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "encodeBuild" $ do
        it "encodes correctly" $ do
            let expected = T.concat
                    [ "{"
                    ,   "\"source_blob\":{"
                    ,     "\"url\":\"https://example.com\","
                    ,     "\"version\":\"abc123\""
                    ,   "}"
                    , "}"
                    ]

                actual = encodeBuild $ Build
                    undefined undefined (SourceBlob "https://example.com" "abc123")

            actual `shouldBe` expected

    describe "decodeBuild" $ do
        it "decodes correctly" $ do
            let response = T.concat
                    [ "{"
                    , "  \"created_at\": \"2014-05-09T19:43:56+00:00\","
                    , "  \"id\": \"22f637e2-05ab-4e71-bb8a-8ea65c88577b\","
                    , "  \"source_blob\": {"
                    , "    \"url\": \"https://example.com\","
                    , "    \"version\": \"abc123\""
                    , "    },"
                    , "  \"slug\": {"
                    , "    \"id\": null"
                    , "    },"
                    , "  \"status\": \"pending\","
                    , "  \"updated_at\": \"2014-05-09T19:43:56+00:00\","
                    , "  \"user\": {"
                    , "    \"email\": \"andy@heroku.com\","
                    , "    \"id\": \"xxxxxxxxxxxxxxxxxxx\""
                    , "  }"
                    , "}"
                    ]

                expected = Right $ Build
                    "22f637e2-05ab-4e71-bb8a-8ea65c88577b"
                    Pending
                    (SourceBlob "https://example.com" "abc123")

            decodeBuild response `shouldBe` expected
