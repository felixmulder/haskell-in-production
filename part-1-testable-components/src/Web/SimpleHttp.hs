-- | This module supplies a very simplified HTTP interface for the purposes of
--   illustrating how to build production grade haskell services.
--
--   The focus for this illustration is not on mastering a specific API, rather
--   techniques like dependency injection and parameterization of components.
--
--   Ergo - don't think too hard on what's in this module, it's for
--   illustration purpoeses.
module Web.SimpleHttp
  -- * Functions
  ( run
  , matches

  -- * Type classes
  , ToResponse(..)
  ) where

import           Prelude

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import           Network.HTTP.Types.Status
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai as Wai (Application, Request, Response)
import           Network.Wai (requestMethod, strictRequestBody, rawPathInfo, responseLBS)

import           Web.SimpleHttp.Class
import           Web.SimpleHttp.Types

matches :: Text -> Path -> Maybe [Text]
matches fullPattern fullPath =
  findAll (splits fullPattern) (splits fullPath)
  where
    splits = T.splitOn "/"

    findAll :: [Text] -> [Text] -> Maybe [Text]
    findAll (pat : patRest) (path : pathRest) =
      if ":" `T.isPrefixOf` pat then do
        rest <- findAll patRest pathRest
        pure $ path : rest
      else if pat == path then do
        findAll patRest pathRest
      else
        Nothing
    findAll [] [] = Just []
    findAll _ _ = Nothing

run :: Int -> (Request -> IO Response) -> IO ()
run port handler = Warp.run port (toApplication handler)
  where
    toApplication :: (Request -> IO Response) -> Wai.Application
    toApplication f wreq respond = do
      req <- toReq wreq
      f req >>= respond . toWres

    toReq :: Wai.Request -> IO Request
    toReq r = do
      rawBody <- strictRequestBody r
      let
        path = T.decodeUtf8 (rawPathInfo r)
        method = case requestMethod r of
          "CONNECT" -> CONNECT
          "DELETE" -> DELETE
          "GET" -> GET
          "HEAD" -> HEAD
          "OPTIONS" -> OPTIONS
          "PATCH" -> PATCH
          "POST" -> POST
          "PUT" -> PUT
          "TRACE" -> TRACE
          m -> error $ "Unsupported method not added, add it if you want to: " <> show m
      print path
      pure Request
        { methodAndPath = method path
        , body = toStrict . LT.decodeUtf8 $ rawBody
        }

    toWres :: Response -> Wai.Response
    toWres NoResponse = responseLBS status404 [] mempty
    toWres (Response status body) =
      responseLBS status [] . LT.encodeUtf8 . fromStrict $ body
