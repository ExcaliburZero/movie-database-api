{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Database.WebAPI.Movies (
    createDatabase
  , serveDatabase
) where

import           Data.Proxy
import           Database.HDBC as HDBC
import           Database.HDBC.Sqlite3
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.API
import           Servant.Server
import           System.Directory (doesFileExist)

import Database.WebAPI.Movies.Queries
import Database.WebAPI.Movies.Types

type MovieAPI =
       "movies" :> Get '[JSON] [Movie]
  :<|> "movies" :> Capture "start" Int :> Capture "end" Int :> Get '[JSON] [Movie]
  :<|> "movies" :> "id" :> Capture "id" String :> Get '[JSON] (Maybe Movie)
  :<|> "movies" :> "title" :> Capture "title" String :> Get '[JSON] [Movie]
  :<|> "movies" :> "actor" :> Capture "actor" String :> Get '[JSON] [Movie]
  :<|> "movies" :> "related" :> Capture "related" String :> Get '[JSON] [Movie]
  :<|> "related" :> Capture "movie1" String :> Capture "movie2" String :> Get '[JSON] Bool
  :<|> "actors" :> "movie_id" :> Capture "movie_id" String :> Get '[JSON] [Actor]
  :<|> "genres" :> "movie_id" :> Capture "movie_id" String :> Get '[JSON] [Genre]

app :: FilePath -> Application
app databaseFile = serve (Proxy :: Proxy MovieAPI)
  $    getAllMovies      databaseFile
  :<|> getSelectedMovies databaseFile
  :<|> getMovieById      databaseFile
  :<|> getMoviesByTitle  databaseFile
  :<|> getMoviesByActor  databaseFile
  :<|> getRelatedMovies  databaseFile
  :<|> addRelatedMovies  databaseFile
  :<|> getActorsByMovie  databaseFile
  :<|> getGenresByMovie  databaseFile

-- | Serves the given database on the given port.
serveDatabase :: FilePath -> Int -> IO ()
serveDatabase databaseFile port = do
  databaseExists <- doesFileExist databaseFile
  if databaseExists
    then do
      putStrLn $ unwords ["Serving database", show databaseFile
                         , "on port", show port, "..."]
      Warp.run port (app databaseFile)
    else
      putStrLn $ "The given database file " ++ show databaseFile ++ " does not exist."

-- | Creates an empty movie database at the given filepath.
createDatabase :: FilePath -> IO ()
createDatabase databaseFile = do
    databaseExists <- doesFileExist databaseFile
    if databaseExists
      then
        putStrLn $ show databaseFile ++ " already exists."
      else do
        putStrLn $ "Creating database at " ++ show databaseFile ++ "..."
        databaseConnection <- connectSqlite3 databaseFile
        mapM_ (createSingleDatabase databaseConnection) [
            createMovieTable
          , createActedInTable
          , createMovieTypesTable
          , createRelatedMoviesTable
          ]
        commit databaseConnection
        disconnect databaseConnection
        putStrLn "Database successfully created!"
  where
    createSingleDatabase conn command = HDBC.run conn command []

-- | Constructs a table creation command from the given table name and
-- contents.
--
-- >>> createTableCommand "Actors" "actor_name Text"
-- "CREATE TABLE Actors (actor_name Text);"
createTableCommand :: String -> String -> String
createTableCommand name contents = "CREATE TABLE " ++ name ++ " (" ++ contents ++ ");"

-- | The SQL command used to create the Movie table.
createMovieTable :: String
createMovieTable = createTableCommand "Movie" $ unlist [
    "movie_id Text"
  , "movie_title Text"
  , "movie_director Text"
  , "movie_year Int"
  , "movie_rating Decimal"
  , "PRIMARY KEY (movie_id)"
  ]

-- | The SQL command used to create the ActedIn table.
createActedInTable :: String
createActedInTable = createTableCommand "ActedIn" $ unlist [
    "actor_name Text"
  , "movie_id Text"
  , "PRIMARY KEY (actor_name, movie_id)"
  , "FOREIGN KEY (actor_name) REFERENCES Actor(actor_name)"
  , "FOREIGN KEY (movie_id) REFERENCES Movie(movie_id)"
  ]

-- | The SQL command used to create the MovieTypes table.
createMovieTypesTable :: String
createMovieTypesTable = createTableCommand "MovieTypes" $ unlist [
    "movie_id Text"
  , "genre_name Text"
  , "PRIMARY KEY (movie_id, genre_name)"
  , "FOREIGN KEY (movie_id) REFERENCES Movie(movie_id)"
  , "FOREIGN KEY (genre_name) REFERENCES Genre(genre_name)"
  ]

-- | The SQL command used to create the RelatedMovies table.
createRelatedMoviesTable :: String
createRelatedMoviesTable = createTableCommand "RelatedMovies" $ unlist [
    "movie_id_1 Text"
  , "movie_id_2 Text"
  , "PRIMARY KEY (movie_id_1, movie_id_2)"
  , "FOREIGN KEY (movie_id_1) REFERENCES Movie(movie_id)"
  , "FOREIGN KEY (movie_id_2) REFERENCES Movie(movie_id)"
  ]

-- | Converts the given list of Strings into a comma separated list String.
--
-- >>> unlist ["actor_name Text"]
-- "actor_name Text"
--
-- >>> unlist ["movie_id Text", "movie_title Text"]
-- "movie_id Text, movie_title Text"
unlist :: [String] -> String
unlist []       = ""
unlist (a:[])   = a
unlist (a:b:xs) = a ++ ", " ++ unlist (b:xs)
