{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module Database.WebAPI.Movies.Docs where

import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Data.Proxy
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.Docs
import Servant.Server

import Database.WebAPI.Movies
import Database.WebAPI.Movies.Types

apiDocs :: API
apiDocs = docs (Proxy :: Proxy MovieAPI)

instance ToSample Movie where
  toSamples _ = [
      (fromString "Terminator", Movie {
          movie_id = "tt1994570"
        , movie_title = "Terminator"
        , movie_director = "Ryan McDonald"
        , movie_year = 2001
        , movie_rating = 4.8
      })
    ]

instance ToCapture (Capture "id" String) where
  toCapture _ =
    DocCapture "id"
               "id of the movie"

instance ToCapture (Capture "title" String) where
  toCapture _ =
    DocCapture "title"
               "title of the movies"

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] (Proxy :: Proxy MovieAPI)

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

type DocsAPI = Raw

api :: Proxy DocsAPI
api = Proxy

server :: Server DocsAPI
server = serveDocs

  where serveDocs _ respond =
          respond $ responseLBS ok200 [plain] docsBS

        plain = (fromString "Content-Type", fromString "text/plain")

app :: Application
app = serve api server
