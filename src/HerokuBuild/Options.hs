module HerokuBuild.Options
    ( Options(..)
    , Command(..)
    , withOptions
    ) where

import Options.Applicative

data Options = Options Command

data Command = Start | Status

withOptions :: (Options -> IO ()) -> IO ()
withOptions f = execParser prog >>= f

prog :: ParserInfo Options
prog = info (helper <*> opts) $
    fullDesc <> progDesc "Interact with the heroku build API"

-- TODO: collect any global options
opts :: Parser Options
opts = subparser $
    command "start"  (info startOptions $
        progDesc "Start a build on the compilation app")
    <> command "status" (info statusOptions $
        progDesc "Check the status of a build")

-- TODO: collect any start-specific options
startOptions :: Parser Options
startOptions = pure $ Options Start

-- TODO: collect any status-specific options
statusOptions :: Parser Options
statusOptions = pure $ Options Status
