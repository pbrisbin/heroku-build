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
withOptions f = f =<< execParser
    (parseOptions `withInfo` "Interact with the heroku build API")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: Parser Options
parseOptions = Options <$> parseApp <*> parseCommand

parseApp :: Parser String
parseApp = strOption $
    short 'a' <> long "app" <> metavar "COMPILE-APP" <>
    help "Heroku app on which to compile"

parseCommand :: Parser Command
parseCommand = subparser $
    command "start" (parseStart `withInfo` "Start a build on the compilation app") <>
    command "status" (parseStatus `withInfo` "Check the status of a build") <>
    command "release" (parseRelease `withInfo` "Release a successful build")

parseStart :: Parser Command
parseStart = Start
    <$> fmap T.pack (argument str (metavar "SOURCE-URL"))
    <*> fmap T.pack (argument str (metavar "VERSION"))

parseStatus :: Parser Command
parseStatus = Status <$> argument str (metavar "BUILD-ID")

parseRelease :: Parser Command
parseRelease = Release
    <$> argument str (metavar "BUILD-ID")
    <*> argument str (metavar "RELEASE-APP")
