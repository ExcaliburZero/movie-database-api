module Database.WebAPI.Movies.Queries (
    getAllMovies
) where

import Control.Monad.Trans.Except (ExceptT(..))
import Database.HDBC as HDBC
import Database.HDBC.Sqlite3
import Servant.Server (Handler())

import Database.WebAPI.Movies.Types

-- | A Handler which returns all of the Movies in the given database.
getAllMovies :: FilePath -> Handler [Movie]
getAllMovies databaseFile = createHandler $ fmap (map sqlToMovie) $ queryDatabase databaseFile "SELECT * FROM Movie;"

-- | Runs the given query on the given database and returns the resulting
-- values.
queryDatabase :: FilePath -> String -> IO [[SqlValue]]
queryDatabase databaseFile queryString = do
  databaseConnection <- connectSqlite3 databaseFile
  items <- quickQuery' databaseConnection queryString []
  disconnect databaseConnection
  return items

-- | Creates an api handler from an IO value.
createHandler :: IO a -> Handler a
createHandler contents = ExceptT (fmap Right contents)
