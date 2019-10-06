module Web.SimpleHttp.Types
  -- * Data types
  ( Request(..)
  , Response(..)
  , MethodAndPath(..)
  , Error(..)
  -- * Type aliases
  , RequestBody
  , Path
  ) where

import Prelude

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (Status)

data Request = Request
  { methodAndPath :: MethodAndPath
  , body :: RequestBody
  }
  deriving stock (Generic, Show)

type Path = Text

type RequestBody = Text

data Error
  = InternalError Text
  | BadRequest Text

data MethodAndPath
  = CONNECT Path
  | DELETE Path
  | GET Path
  | HEAD Path
  | OPTIONS Path
  | PATCH Path
  | POST Path
  | PUT Path
  | TRACE Path
  deriving stock (Show)

data Response
  = NoResponse
  | Response
    { statusCode :: Status
    , body :: Text
    }
  deriving stock (Generic)

