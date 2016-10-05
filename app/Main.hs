-- |
-- Module      : Main
-- Description : Implements an application for hosting a web api of a
--               movie database.
-- Copyright   : (c) Christopher Wells, 2016
-- License     : MIT
-- Maintainer  : cwellsny@nycap.rr.com
module Main where

import Console.Options
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Version (makeVersion)
import Text.Read (readMaybe)

import Database.WebAPI.Movies

main :: IO ()
main = defaultMain $ do
  programName "movie-database-api"
  programDescription "An application for generating and serving a movie database."
  programVersion $ makeVersion [0,1,0]

  command "serve" $ do
    description "Serves the given database as a web api."

    databaseFile <- databaseFileArgument
    portNumber <- flagParam
      (FlagLong "port" <> FlagDescription "The port to serve the api on.")
      (FlagRequired portNumberParser)

    action $ \toParam -> do
      let databasePath = toParam databaseFile
      let port = fromMaybe defaultPort (toParam portNumber)
      putStrLn $ unwords ["Serving database", show databasePath
                         , "on port", show port, "..."]
      serveDatabase databasePath port

  command "create" $ do
    description "Creates an empty database at the given filepath."

    databaseFile <- databaseFileArgument

    action $ \toParam -> do
      let databasePath = toParam databaseFile
      putStrLn $ "Creating database at " ++ show databasePath ++ "..."
      createDatabase databasePath
      putStrLn "Database successfully created!"

-- | A parser for parsing the database file argument.
databaseFileArgument :: OptionDesc r (Arg String)
databaseFileArgument = argument "databaseFile" Right

-- | The default port number used for the web api.
defaultPort :: Int
defaultPort = 8001

-- | Parser for the port number flag argument.
portNumberParser :: String -> Either String Int
portNumberParser x = case (readMaybe :: String -> Maybe Int) x of
  Just n  -> Right n
  Nothing -> Left $ "Invalid port number " ++ x
