{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Database.WebAPI.Movies (
    serveDatabase
) where

import           Data.Proxy
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.API
import           Servant.Server

import Database.WebAPI.Movies.Types

type MovieAPI = "movie" :> Get '[JSON] [Movie]

-- | Serves the movies in the given database.
movieServer :: FilePath -> Handler [Movie]
movieServer databaseFile = return [
      Movie { movieName = "Terminator" }
    , Movie { movieName = "Terminator2" }
  ]

app :: FilePath -> Application
app databaseFile = serve (Proxy :: Proxy MovieAPI) $ movieServer databaseFile

-- | Serves the given database on the given port.
serveDatabase :: FilePath -> Int -> IO ()
serveDatabase databaseFile port = Warp.run port (app databaseFile)
