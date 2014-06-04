module HerokuBuild.Options
    ( Options(..)
    , Command(..)
    , withOptions
    ) where

import Options.Applicative
import Data.Text (Text)
import qualified Data.Text as T

data Options = Options Command

data Command = Start Text Text | Status Text

withOptions :: (Options -> IO ()) -> IO ()
withOptions f = execParser prog >>= f

prog :: ParserInfo Options
prog = info (helper <*> opts) $
    fullDesc <> progDesc "Interact with the heroku build API"

-- TODO: global options
opts :: Parser Options
opts = subparser $
    command "start"  (info startOptions $
        progDesc "Start a build on the compilation app")
    <> command "status" (info statusOptions $
        progDesc "Check the status of a build")

startOptions :: Parser Options
startOptions = Options <$> (Start
    <$> fmap T.pack (argument str (metavar "SOURCE-URL"))
    <*> fmap T.pack (argument str (metavar "VERSION")))

statusOptions :: Parser Options
statusOptions = Options <$> (Status
    <$> fmap T.pack (argument str (metavar "BUILD-ID")))
