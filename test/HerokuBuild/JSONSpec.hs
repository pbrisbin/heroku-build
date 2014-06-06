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
            let encoded = encode $ newBuild "https://example.com" "abc123"

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
                    , "    \"id\": \"361d1603-1fb4-4189-a3d7-cbc46967e21f\""
                    , "    },"
                    , "  \"status\": \"pending\","
                    , "  \"updated_at\": \"2014-05-09T19:43:56+00:00\","
                    , "  \"user\": {"
                    , "    \"email\": \"andy@heroku.com\","
                    , "    \"id\": \"xxxxxxxxxxxxxxxxxxx\""
                    , "  }"
                    , "}"
                    ]

            fmap status decoded `shouldBe` Just Pending
            fmap slug decoded `shouldBe`
                Just (Slug (Just "361d1603-1fb4-4189-a3d7-cbc46967e21f"))

    describe "Slug" $ do
        it "encodes correctly" $ do
            let encoded = encode $ Slug (Just "some-slug-id")

            encoded `shouldBe` "{\"slug\":\"some-slug-id\"}"
