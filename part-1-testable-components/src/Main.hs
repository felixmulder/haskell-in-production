{-# Language DuplicateRecordFields #-}
module Main
  ( main

  -- * Data types
  , UserId(..)
  , UserName(..)
  , Password(..)

  -- * Interfaces
  , Persist(..)
  , Log(..)
  ) where

import Prelude

import Control.Lens ((^.))
import Control.Monad.Reader (ReaderT, runReaderT, lift)
import Control.Monad.Reader.Class (asks)
import Data.Functor ((<&>))
import Data.Has (Has(..))
import Data.String (IsString)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Web.SimpleHttp
import Data.Generics.Labels ()

main :: IO ()
main = run 8080 $ \req -> runReaderT (api req) app
  where
    app :: Application IO
    app = Application
      { persistence = Persistence
        { _persistUser = \_ _ -> pure (UserId "User1")
        , _deleteUser = \_ -> pure True
        }
      , logger = Logger (putStrLn . unpack)
      }

api ::
     Log m
  => Persist m
  => Request -> m Response
api request =
  case methodAndPath request of
    POST (matches "/user" -> Just []) ->
      createNewUser (request ^. #body) <&> toResponseFrom
    DELETE (matches "/user/:userId" -> Just [userId]) -> do
      deleteUser (UserId userId) <&> toResponseFrom
    _ ->
      pure NoResponse

createNewUser ::
     Persist m
  => Log m
  => RequestBody -> m (Either Error User)
createNewUser body =
  case bodyToUser body of
    Left err ->
      Left err <$ logLn ("Couldn't convert " <> body <> "to user and pass")
    Right (user, pass) -> do
      logLn $ "Going to create " <> unUserName user
      userId <- persistUser user pass
      -- Create a response from the persisted argument:
      pure . Right $ User { userName = user, userId = userId }

bodyToUser :: RequestBody -> Either Error (UserName, Password)
bodyToUser = undefined

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------
newtype UserName =
  UserName { unUserName :: Text }
  deriving newtype (IsString, Loggable)

newtype Password =
  Password { unPassword :: Text }
  deriving newtype (IsString, Loggable)

newtype UserId =
  UserId { unUserId :: Text }
  deriving newtype (IsString, Loggable)

data User = User
  { userId :: UserId
  , userName :: UserName
  }

instance ToResponse User where
  toResponseFrom = undefined

data Application m = Application
  { persistence :: Persistence m
  , logger :: Logger m
  }
  deriving stock (Generic)

instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a { logger = f . logger $ a }

instance Has (Persistence m) (Application m) where
  getter = persistence
  modifier f a = a { persistence = f . persistence $ a }

--------------------------------------------------------------------------------
-- Persistence interface
--------------------------------------------------------------------------------
class Monad m => Persist m where
  persistUser :: UserName -> Password -> m UserId
  deleteUser :: UserId -> m Bool

data Persistence m = Persistence
  { _persistUser :: UserName -> Password -> m UserId
  , _deleteUser :: UserId -> m Bool
  }

instance
  ( Has (Persistence m) r
  , Monad m
  ) => Persist (ReaderT r m) where
  persistUser user pass =
    asks getter >>= \(Persistence persist _) -> lift $ persist user pass
  deleteUser userId =
    asks getter >>= \(Persistence _ get) -> lift $ get userId

--------------------------------------------------------------------------------
-- Logging interface
--------------------------------------------------------------------------------
class Loggable a where
  fromLoggable :: a -> Text

class Monad m => Log m where
  logLn :: HasCallStack => Loggable a => a -> m ()

data Logger m = Logger
  { _logLn :: HasCallStack => Text -> m ()
  }

instance
  ( Has (Logger m) r
  , Monad m
  ) => Log (ReaderT r m) where
  logLn a =
    asks getter >>= \(Logger doLog) -> lift . doLog . fromLoggable $ a

instance Loggable Text where
  fromLoggable = id
