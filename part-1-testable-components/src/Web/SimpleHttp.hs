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

  -- * Data types
  , Request(..)
  , Response(..)
  , MethodAndPath(..)
  , Error(..)

  -- * Type aliases
  , RequestBody
  , Path

  -- * Type classes
  , ToResponse(..)
  ) where

import           Prelude

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Status
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai as Wai (Application, Request, Response)
import           Network.Wai (requestMethod, strictRequestBody, rawPathInfo, responseLBS)

data Request = Request
  { methodAndPath :: MethodAndPath
  , body :: RequestBody
  }
  deriving stock (Generic)

type Path = Text

type RequestBody = Text

data Error
  = InternalError Text
  | BadRequest Text

data MethodAndPath
  = POST Path
  | DELETE Path

data Response
  = NoResponse
  | Response
    { statusCode :: Status
    , body :: Text
    }
  deriving stock (Generic)

class ToResponse a where
  toResponseFrom :: a -> Response

instance ToResponse Bool where
  toResponseFrom = \case
    False ->
      Response status200 mempty
    True ->
      Response status404 mempty

instance ToResponse a => ToResponse (Either Error a) where
  toResponseFrom = \case
    Left (InternalError e) -> Response status500 e
    Left (BadRequest e) -> Response status400 e
    Right a -> toResponseFrom a

matches :: Text -> Path -> Maybe [Text]
matches fullPattern fullPath =
  findAll (splits fullPattern) (splits fullPath)
  where
    splits = T.splitOn "/"

    findAll :: [Text] -> [Text] -> Maybe [Text]
    findAll (pat : patRest) (path : pathRest) =
      if ":" `T.isPrefixOf` pat || pat == path then do
        rest <- findAll patRest pathRest
        pure $ path : rest
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
          "POST" -> POST
          "DELETE" -> DELETE
          m -> error $ "Unsupported method not added, add it if you want to: " <> show m
      pure $ Request
        { methodAndPath = method path
        , body = toStrict . LT.decodeUtf8 $ rawBody
        }

    toWres :: Response -> Wai.Response
    toWres NoResponse = responseLBS status404 [] mempty
    toWres (Response status body) =
      responseLBS status [] . LT.encodeUtf8 . fromStrict $ body
