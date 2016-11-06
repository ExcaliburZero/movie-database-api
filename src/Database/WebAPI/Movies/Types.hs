{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Database.WebAPI.Movies.Types (
    Movie(..)
  , Actor(..)

  , sqlToMovie
  , sqlToSingleMovie
  , sqlToActor
) where

import Data.Aeson
import Data.ByteString.Char8 (unpack)
import Database.HDBC.Types (SqlValue(..))
import GHC.Generics

data Movie = Movie {
    movie_id       :: String
  , movie_title    :: String
  , movie_director :: String
  , movie_year     :: Int
  , movie_rating   :: Double
  } deriving (Eq, Generic, Show, ToJSON)

sqlToMovie :: [SqlValue] -> Movie
sqlToMovie (SqlByteString idString:SqlByteString title:SqlByteString director:SqlInt64 year:SqlDouble rating:[]) = Movie {
    movie_id       = unpack idString
  , movie_title    = unpack title
  , movie_director = unpack director
  , movie_year     = fromIntegral year
  , movie_rating   = rating
}
sqlToMovie x = error $ "Incorrectly formed Movie sql: " ++ show x

sqlToSingleMovie :: [[SqlValue]] -> Maybe Movie
sqlToSingleMovie sql = case sql of
  [] -> Nothing
  x  -> Just . sqlToMovie . head $ x

data Actor = Actor {
  actor_name :: String
} deriving (Eq, Generic, Show, ToJSON)

sqlToActor :: [SqlValue] -> Actor
sqlToActor (SqlByteString name:[]) = Actor {actor_name = unpack name}
sqlToActor x = error $ "Incorrectly formed Movie sql: " ++ show x
