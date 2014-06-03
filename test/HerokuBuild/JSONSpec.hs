module HerokuBuild.JSONSpec (main, spec) where

import HerokuBuild.JSON
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BS
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Build" $ do
        it "encodes correctly" $ do
            let encoded = encode $ Build undefined undefined
                    (SourceBlob "https://example.com" "abc123")

            encoded `shouldBe` BS.concat
                [ "{"
                ,   "\"source_blob\":{"
                ,     "\"url\":\"https://example.com\","
                ,     "\"version\":\"abc123\""
                ,   "}"
                , "}"
                ]

        it "decodes correctly" $ do
            let decoded = decode $ BS.concat
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

            decoded `shouldBe` (Just $ Build
                "22f637e2-05ab-4e71-bb8a-8ea65c88577b" Pending
                (SourceBlob "https://example.com" "abc123"))
