-- |
-- Module      : Main
-- Description : Implements an application for hosting a web api of a
--               movie database.
-- Copyright   : (c) Christopher Wells, 2016
-- License     : MIT
-- Maintainer  : cwellsny@nycap.rr.com
module Main where

import Console.Options

import Database.WebAPI.Movies

main :: IO ()
main = defaultMain $ do
  programName "movie-database-api"
  command "serve" $ do
    databaseFile <- databaseFileArgument
    action $ \toParam ->
      putStrLn $ unwords ["Serving database", show (toParam databaseFile)
                         , "on port", show defaultPort, "..."]

-- | A parser for parsing the database file argument.
databaseFileArgument :: OptionDesc r (Arg String)
databaseFileArgument = argument "databaseFile" Right

-- | The default port number used for the web api.
defaultPort :: Int
defaultPort = 8001
