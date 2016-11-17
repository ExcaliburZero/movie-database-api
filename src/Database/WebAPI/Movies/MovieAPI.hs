{-# LANGUAGE ScopedTypeVariables #-}
module Database.WebAPI.Movies.MovieAPI (
  queryMovieByTitle

  , omdbUrl, queryOmdb, parseMovieJson, parseMovie
) where

import Data.Aeson
import Data.Aeson.Types
import Data.List.Split (splitOn)
import Data.List.Utils (replace)
import Data.String (fromString)
import Text.Read (readMaybe)
import Network.HTTP

import Database.WebAPI.Movies.Types

-- | Queries omdb to get the Movie info and Actor and Genre relationships for
-- the movie with the given title.
queryMovieByTitle :: String -> IO (Maybe (Movie, [(String, Actor)], [(String, Genre)]))
queryMovieByTitle movieTitle = do
  movieJson <- queryOmdb movieTitle
  return $ parseMovieJson movieJson

-- | Returns an Omdb query url for the given movie title.
omdbUrl :: String -> String
omdbUrl title = "http://www.omdbapi.com/?t=" ++ (replace " " "%20" title) ++ "&y=&plot=short&r=json"

-- | Queries the Ombd Api using the given movie title and returns the json
-- response.
--queryOmdb :: String -> IO (Maybe Value)
queryOmdb :: String -> IO String
queryOmdb title = do
  let url = omdbUrl title
  rsp <- simpleHTTP (getRequest url)
  text <- getResponseBody rsp
  return text

-- | Attempts to parse the given Movie json string into a Movie.
parseMovieJson :: String -> Maybe (Movie, [(String, Actor)], [(String, Genre)])
parseMovieJson jsonText = do
  results <- decode . fromString $ jsonText
  movie <- parseMovie results
  actors <- parseActors results
  genres <- parseGenres results
  return (movie, actors, genres)

-- | Attempts to parse the given json into a Movie.
parseMovie :: Object -> Maybe Movie
parseMovie results = do
  strings <- flip parseMaybe results $ \obj -> do 
    mId       <- obj .: fromString "imdbID"
    title     <- obj .: fromString "Title"
    director  <- obj .: fromString "Director"
    yearStr   <- obj .: fromString "Year"
    ratingStr <- obj .: fromString "imdbRating"
    return [mId, title, director, yearStr, ratingStr]
  year   <- readMaybe (strings !! 3) :: Maybe Int
  rating <- readMaybe (strings !! 4) :: Maybe Double
  return Movie {
      movie_id       = strings !! 0
    , movie_title    = strings !! 1
    , movie_director = strings !! 2
    , movie_year     = year
    , movie_rating   = rating
    }

-- | Attempts to parse the given json into Actor relationships.
parseActors :: Object -> Maybe [(String, Actor)]
parseActors results = do
  strings <- flip parseMaybe results $ \obj -> do 
    mId       <- obj .: fromString "imdbID"
    actorsStr <- obj .: fromString "Actors"
    let actors = splitOn ", " actorsStr
    return (mId, actors)
  let actorNames = snd strings
  let movieId = fst strings
  let actorPairs = map (\a -> (movieId, Actor a)) actorNames
  return actorPairs

-- | Attempts to parse the given json into Genre relationships.
parseGenres :: Object -> Maybe [(String, Genre)]
parseGenres results = do
  strings <- flip parseMaybe results $ \obj -> do 
    mId       <- obj .: fromString "imdbID"
    genresStr <- obj .: fromString "Genre"
    let genres = splitOn ", " genresStr
    return (mId, genres)
  let genreNames = snd strings
  let movieId = fst strings
  let genrePairs = map (\a -> (movieId, Genre a)) genreNames
  return genrePairs
