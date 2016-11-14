module Database.WebAPI.Movies.Queries (
    getAllMovies
  , getSelectedMovies
  , getMovieById
  , getMoviesByTitle
  , getMoviesByActor
  , getMoviesSearch
  , getRelatedMovies
  , deleteMovieById

  , addRelatedMovies

  , getAllGenres
  , getGenresByMovie

  , getActorsByMovie

  , queryDatabase
  , insertDatabase
  , createHandler
) where

import Control.Monad (liftM2)
import Control.Monad.Trans.Except (ExceptT(..))
import Data.ByteString.Char8 (unpack)
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
    movieTitleQuery = "SELECT * FROM Movie WHERE movie_title LIKE '%' || ? || '%' ORDER BY movie_title ASC"

-- | A Handler which returns all of the Movies with the given actor.
getMoviesByActor :: FilePath -> String -> Handler [Movie]
getMoviesByActor databaseFile actor = createHandler selectedMovies
  where
    selectedMovies  = fmap (map sqlToMovie) queryResults
    queryResults    = queryDatabase databaseFile movieActorQuery [SqlString actor]
    movieActorQuery = "SELECT Movie.movie_id, movie_title, movie_director, movie_year, movie_rating FROM Movie INNER JOIN ActedIn ON (Movie.movie_id = ActedIn.movie_id) WHERE actor_name LIKE '%' || ? || '%'"

-- | A Handler which returns all of the Movies that meet the given search filters.
getMoviesSearch :: FilePath -> String -> String -> String -> String -> Handler [Movie]
getMoviesSearch databaseFile term sType genre rating = createHandler selectedMovies
  where
    selectedMovies   = fmap (map sqlToMovie) queryResults
    queryResults     = queryDatabase databaseFile movieSearchQuery [SqlString term]
    movieSearchQuery = "SELECT Movie.movie_id, movie_title, movie_director, movie_year, movie_rating FROM Movie INNER JOIN ActedIn ON (Movie.movie_id = ActedIn.movie_id) INNER JOIN MovieTypes ON (Movie.movie_id = MovieTypes.movie_id) WHERE " ++ termWhere ++ genreWhere ++ ratingWhere ++ "GROUP BY Movie.movie_id ORDER BY movie_title ASC"
    termWhere   = if sType == "Actor"
                  then "actor_name LIKE '%' || ? || '%' "
                  else "movie_title LIKE '%' || ? || '%' "
    genreWhere  = if genre /= "Any"
                  then "AND genre_name = '" ++ genre ++ "' "
                  else ""
    ratingWhere = if rating /= "Any"
                  then "AND movie_rating BETWEEN " ++ rating ++ " AND " ++ show ((read :: String -> Int) rating + 1) ++ " "
                  else ""

-- | A Handler which returns all of the movies related to the given movie.
getRelatedMovies :: FilePath -> String -> Handler [Movie]
getRelatedMovies databaseFile movieId = createHandler selectedMovies
  where
    selectedMovies = do
      results <- fmap ((map . map) sqlToMovie) queryResults0
      return $ map head $ filter (\x -> not (null x)) results
    queryResults0  = do
      ids <- selectedIds
      databaseConnection <- connectSqlite3 databaseFile
      let elements = map (\x -> [x]) $ map SqlString ids
      items <- mapM (quickQuery' databaseConnection movieIdQuery) elements
      disconnect databaseConnection
      return items
    movieIdQuery   = "SELECT * FROM Movie WHERE movie_id = ? ORDER BY movie_title ASC"
    selectedIds    = fmap (map extractId) $ liftM2 (++) queryResults1 queryResults2
    queryResults1  = queryDatabase databaseFile relatedMoviesQuery1 [SqlString movieId]
    relatedMoviesQuery1 = "SELECT movie_id_2 FROM RelatedMovies WHERE movie_id_1 = ?"
    queryResults2 = queryDatabase databaseFile relatedMoviesQuery2 [SqlString movieId]
    relatedMoviesQuery2 = "SELECT movie_id_1 FROM RelatedMovies WHERE movie_id_2 = ?"
    extractId ([SqlByteString i]) = unpack i
    extractId _ = error "Bad"

-- | A Handler which deletes the Movie with the given id.
deleteMovieById :: FilePath -> String -> Handler Bool
deleteMovieById databaseFile movieId = createHandler deleteAction
  where
    deleteAction = do
      databaseConnection <- connectSqlite3 databaseFile
      _ <- run databaseConnection deleteStringMovie   [SqlString movieId]
      _ <- run databaseConnection deleteStringActor   [SqlString movieId]
      _ <- run databaseConnection deleteStringGenre   [SqlString movieId]
      _ <- run databaseConnection deleteStringRelated $ map SqlString [movieId, movieId]
      commit databaseConnection
      disconnect databaseConnection
      return True
    deleteStringMovie   = "DELETE FROM Movie WHERE movie_id = ?"
    deleteStringActor   = "DELETE FROM ActedIn WHERE movie_id = ?"
    deleteStringGenre   = "DELETE FROM MovieTypes WHERE movie_id = ?"
    deleteStringRelated = "DELETE FROM RelatedMovies WHERE movie_id_1 = ? || movie_id_2 = ?"

-- | A Handler which adds a set of related movies.
addRelatedMovies :: FilePath -> String -> String -> Handler Bool
addRelatedMovies databaseFile movie1 movie2 = createHandler insertAction
  where
    insertAction = insertDatabase databaseFile insertString elements
    elements     = map SqlString [movie1, movie2]
    insertString = "INSERT INTO RelatedMovies VALUES (?,?)"

-- | A Handler which returns all of the Actors in the given Movie.
getActorsByMovie :: FilePath -> String -> Handler [Actor]
getActorsByMovie databaseFile movieID = createHandler selectedActors
  where
    selectedActors  = fmap (map sqlToActor) queryResults
    queryResults    = queryDatabase databaseFile actorMovieQuery [SqlString movieID]
    actorMovieQuery = "SELECT actor_name FROM Movie INNER JOIN ActedIn ON (Movie.movie_id = ActedIn.movie_id) WHERE Movie.movie_id = ?"

-- | A Handler which returns a list of all of the movie Genres.
getAllGenres :: FilePath -> Handler [Genre]
getAllGenres databaseFile = createHandler genres
  where
    genres       = fmap (map sqlToGenre) queryResults
    queryResults = queryDatabase databaseFile genresQuery []
    genresQuery  = "SELECT genre_name FROM MovieTypes GROUP BY genre_name"

-- | A Handler which returns all of the Genres for the given Movie.
getGenresByMovie :: FilePath -> String -> Handler [Genre]
getGenresByMovie databaseFile movieID = createHandler selectedGenres
  where
    selectedGenres  = fmap (map sqlToGenre) queryResults
    queryResults    = queryDatabase databaseFile genreMovieQuery [SqlString movieID]
    genreMovieQuery = "SELECT genre_name FROM Movie INNER JOIN MovieTypes ON (Movie.movie_id = MovieTypes.movie_id) WHERE Movie.movie_id = ?"

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
