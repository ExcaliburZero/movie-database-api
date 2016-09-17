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

databaseFileArgument :: OptionDesc r (Arg String)
databaseFileArgument = argument "databaseFile" Right

defaultPort :: Int
defaultPort = 8001
