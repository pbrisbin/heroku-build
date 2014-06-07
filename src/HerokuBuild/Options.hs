module HerokuBuild.Options
    ( Options(..)
    , Command(..)
    , withOptions
    ) where

import Options.Applicative
import Data.Text (Text)
import qualified Data.Text as T

data Options = Options String Command

data Command = Start Text Text | Status String | Release String String

withOptions :: (Options -> IO ()) -> IO ()
withOptions f = execParser prog >>= f

prog :: ParserInfo Options
prog = info (helper <*> opts) $
    fullDesc <> progDesc "Interact with the heroku build API"

opts :: Parser Options
opts = Options
    <$> strOption
        (long "app"
      <> short 'a'
      <> metavar "COMPILE-APP"
      <> help "Heroku app on which to compile")
    <*> subparser
        (cmd "start" startOptions "Start a build on the compilation app"
      <> cmd "status" statusOptions "Check the status of a build"
      <> cmd "release" releaseOptions "Release a successful build")

  where
    cmd c o d = command c (info (helper <*> o) $ progDesc d)

startOptions :: Parser Command
startOptions = Start
    <$> fmap T.pack (argument str (metavar "SOURCE-URL"))
    <*> fmap T.pack (argument str (metavar "VERSION"))

statusOptions :: Parser Command
statusOptions = Status <$> argument str (metavar "BUILD-ID")

releaseOptions :: Parser Command
releaseOptions = Release
    <$> argument str (metavar "BUILD-ID")
    <*> argument str (metavar "RELEASE-APP")
