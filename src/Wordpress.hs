{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass, RecordWildCards #-}
module Wordpress (Post (..), createPost, editPostContent, getPost, searchPosts) where

import Control.Lens              ((^.))
import Data.Aeson                (FromJSON (parseJSON), KeyValue ((.=)), encode, object, withObject,
                                  (.:))
import Data.Aeson.Types          (Pair)
import Network.HTTP.Base         (urlEncode)
import Network.HTTP.Client       (Request, parseRequest_, setQueryString)
import Network.HTTP.Simple       (addRequestHeader, getResponseBody, httpJSONEither,
                                  setRequestBodyLBS)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Secrets                   (wpTok)


-- API FUNCTIONS

searchPosts ∷ Text → IO [Post]
searchPosts search
  = wpReq "GET" "/posts" [("search", urlEnc search)]
  & runReq @PostsResp
  <&> (^. #posts)

getPost ∷ Int → IO Post
getPost postId
  = wpReq "GET" ("/posts/" ⊕ show postId) []
  & runReq

editPostContent ∷ Int → (Text → Text) → IO ()
editPostContent postId f = do
  old ← getPost postId <&> (^. #content)
  wpReq "POST" ("/posts/" ⊕ show postId) []
    & setBody [ "content" .= f old ]
    & runReq

createPost ∷ Text → [Text] → Text → IO Post
createPost title categories content
  = wpReq "POST" "/posts/new" []
  & setBody
    [ "categories" .= categories
    , "content" .= content
    , "title" .= title
    ]
  & runReq


-- TYPES

data Post
  = Post
    { postId  ∷ Int
    , title   ∷ Text
    , content ∷ Text
    , url     ∷ Text
    }
  deriving (Eq, Generic, Show)

data PostsResp
  = PostsResp
    { posts ∷ [Post]
    }
  deriving (Eq, FromJSON, Generic, Show)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \obj → do
    postId  ← obj .: "ID"
    title   ← obj .: "title"
    content ← obj .: "content"
    url     ← obj .: "URL"
    pure Post {..}


-- UTILS

siteId ∷ Int
siteId = 223766183

base ∷ Text
base = "https://public-api.wordpress.com/rest/v1.1/sites/" ⊕ show siteId

wpReq ∷ Text → Text → [(Text, Text)] → Request
wpReq method endpoint queryPairs =
  method ⊕ " " ⊕ base ⊕ endpoint
    & toString
    & parseRequest_
    & setQueryString
      ([("http_envelope", Just "False")]
      ⊕ map (\(a,b) → (encodeUtf8 a, Just (encodeUtf8 b))) queryPairs
      )
    & addRequestHeader hAuthorization ("Bearer " ⊕ wpTok)
    & addRequestHeader hContentType "application/json"

runReq ∷ FromJSON a ⇒ Request → IO a
runReq req = do
  resp ← httpJSONEither req
  case getResponseBody resp of
    Left err   → print err ≫ error ""
    Right body → pure body

setBody ∷ [Pair] → Request → Request
setBody = setRequestBodyLBS . encode . object

urlEnc ∷ Text → Text
urlEnc = toText . urlEncode . toString
