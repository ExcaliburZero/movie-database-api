module Database.WebAPI.Movies.Queries (
    getAllMovies
  , getSelectedMovies
  , getMovieById
  , getMoviesByTitle

  , addRelatedMovies

  , queryDatabase
  , insertDatabase
  , createHandler
) where

import Control.Monad.Trans.Except (ExceptT(..))
import Database.HDBC as HDBC
import Database.HDBC.Sqlite3
import Servant.Server (Handler())

import Database.WebAPI.Movies.Types

-- | A Handler which returns all of the Movies in the given database.
getAllMovies :: FilePath -> Handler [Movie]
getAllMovies databaseFile = createHandler $ fmap (map sqlToMovie) $ queryDatabase databaseFile "SELECT * FROM Movie;" []

-- | A Handler which returns the movies between the two given indexes.
--
-- The start index is inclusive, while the end index is exclusive. The start
-- index should be less than the end index, and the start index should be
-- greater than -1.
--
-- If more movies are asked for than are available in the database, then the
-- unavailable movies are not included in the returned list.
getSelectedMovies :: FilePath -> Int -> Int -> Handler [Movie]
getSelectedMovies databaseFile start end =
    fmap takeSelection $ getAllMovies databaseFile
  where
    takeSelection = if   end > start && start > -1
                    then take (end - start) . drop start
                    else const []

-- | A Handler which returns the Movie with the given id.
getMovieById :: FilePath -> String -> Handler (Maybe Movie)
getMovieById databaseFile movieId = createHandler maybeMovie
  where
    maybeMovie   = fmap sqlToSingleMovie queryResults
    queryResults = queryDatabase databaseFile movieIdQuery [SqlString movieId]
    movieIdQuery = "SELECT * FROM Movie WHERE movie_id = ?"

-- | A Handler which returns all of the Movies with the given title.
getMoviesByTitle :: FilePath -> String -> Handler [Movie]
getMoviesByTitle databaseFile title = createHandler selectedMovies
  where
    selectedMovies  = fmap (map sqlToMovie) queryResults
    queryResults    = queryDatabase databaseFile movieTitleQuery [SqlString title]
    movieTitleQuery = "SELECT * FROM Movie WHERE movie_title LIKE '%' || ? || '%'"

addRelatedMovies :: FilePath -> String -> String -> Handler Bool
addRelatedMovies databaseFile movie1 movie2 = createHandler insertAction
  where
    insertAction = insertDatabase databaseFile insertString elements
    elements     = map SqlString [movie1, movie2]
    insertString = "INSERT INTO RelatedMovies VALUES (?,?)"

-- | Runs the given query on the given database and returns the resulting
-- values.
queryDatabase :: FilePath -> String -> [SqlValue] -> IO [[SqlValue]]
queryDatabase databaseFile queryString elements = do
  databaseConnection <- connectSqlite3 databaseFile
  items <- quickQuery' databaseConnection queryString elements
  disconnect databaseConnection
  return items

insertDatabase :: FilePath -> String -> [SqlValue] -> IO Bool
insertDatabase databaseFile insertString elements = do
  databaseConnection <- connectSqlite3 databaseFile
  _ <- run databaseConnection insertString elements
  commit databaseConnection
  disconnect databaseConnection
  return True

-- | Creates an api handler from an IO value.
createHandler :: IO a -> Handler a
createHandler contents = ExceptT (fmap Right contents)
