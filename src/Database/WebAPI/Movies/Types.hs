{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Database.WebAPI.Movies.Types where

import Data.Aeson
import GHC.Generics

data Movie = Movie {
    movie_id       :: String
  , movie_title    :: String
  , movie_director :: String
  , movie_year     :: Int
  , movie_rating   :: Float
  } deriving (Eq, Generic, Show, ToJSON)
