module Types where

import Prelude

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (IsString)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status201, status204, status404)

import Web.SimpleHttp ()
import Web.SimpleHttp.Types (Response(..))
import Web.SimpleHttp.Class (ToResponse(..))

data UserRequest = UserRequest
  { username :: Text
  , password :: Text
  }
  deriving stock (Generic)

instance FromJSON UserRequest

newtype UserName =
  UserName { unUserName :: Text }
  deriving newtype (IsString, ToJSON)

newtype Password =
  Password { unPassword :: Text }
  deriving newtype (IsString, ToJSON)

newtype UserId =
  UserId { unUserId :: Text }
  deriving newtype (IsString, ToJSON)

data Deletion
  = Deleted
  | NotDeleted

instance ToResponse Deletion where
  toResponseFrom = \case
    NotDeleted ->
      Response status404 mempty
    Deleted ->
      Response status204 mempty

data User = User
  { userId :: UserId
  , userName :: UserName
  }
  deriving stock (Generic)

instance ToJSON User

instance ToResponse User where
  toResponseFrom =
    Response status201 . toStrict . decodeUtf8 . encode
