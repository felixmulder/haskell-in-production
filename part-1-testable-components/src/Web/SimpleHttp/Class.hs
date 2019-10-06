module Web.SimpleHttp.Class
  ( ToResponse(..)
  ) where

import Prelude
import Web.SimpleHttp.Types (Response(..), Error(..))
import Network.HTTP.Types.Status (status400, status500)

class ToResponse a where
  toResponseFrom :: a -> Response

instance ToResponse a => ToResponse (Either Error a) where
  toResponseFrom = \case
    Left (InternalError e) -> Response status500 e
    Left (BadRequest e) -> Response status400 e
    Right a -> toResponseFrom a

