{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Database.WebAPI.Movies.Types where

import Data.Aeson
import GHC.Generics

data Movie = Movie {
    movieName :: String
  } deriving (Eq, Generic, Show, ToJSON)
