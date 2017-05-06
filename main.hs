{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString.Char8 as BS

import HerokuBuild

main :: IO ()
main = withOptions $ \(Options app cmd) -> do
    k <- getApiKey

    let builds = "/apps/" ++ app ++ "/builds/"

    case cmd of
        Start u v -> p buildId =<< postHeroku k builds (newBuild u v)
        Status b -> p status =<< getHeroku k (builds ++ b)
        Release b a -> do
            mb <- getHeroku k (builds ++ b)

            case mb of
                Just (Build _ Success _ s) -> do
                    postHeroku' k ("/apps/" ++ a ++ "/releases") s
                    putStrLn "Success"

                _ -> err "API error or build not found or not succeeded"

  where
    p :: Show a => (Build -> a) -> Maybe Build -> IO ()
    p acc = maybe (err "failed to parse API response") (print . acc)

getApiKey :: IO ApiKey
getApiKey = do
    mk <- lookupEnv "HEROKU_API_KEY"

    case mk of
        Just k -> return $ encode k
        Nothing -> err "HEROKU_API_KEY environment variable not set"

  where
    encode :: String -> ApiKey
    encode k =
        let encoded = BS.encode $ ":" `BS.append` BS.pack k
        in "Basic " `BS.append` encoded

err :: String -> IO a
err msg = do
    hPutStrLn stderr $ "Error: " ++ msg
    exitFailure
