module Main
  ( main
  ) where

-- Third party lib imports
import Prelude
import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)
import Data.Aeson
import Data.Functor ((<&>))
import Data.Has (Has(..))
import Data.Text (pack, unpack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Data.Generics.Labels ()

-- Imports from our simplified web framework
import Web.SimpleHttp (matches, run)
import Web.SimpleHttp.Types (Request(..), Response(..), MethodAndPath(..))
import Web.SimpleHttp.Types (RequestBody, Error(..))
import Web.SimpleHttp.Class (toResponseFrom)

-- Domain types and interfaces:
import Types (User(..), UserName(..), UserId(..), Password(..), Deletion(..))
import Types (UserRequest(..))
import Persist (Persist(..), Persistence(..))
import Log (Log(..), Logger(..))

main :: IO ()
main = run 8080 $ \req -> runReaderT (api req) app
  where
    app :: Application IO
    app = Application
      { persistence = Persistence
        { doPersistUser =
            \_ _ -> pure (UserId "User1")
        , doDeleteUser =
            \_ -> pure Deleted
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
    DELETE (matches "/user/:userId" -> Just [userId]) ->
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
bodyToUser =
  either
    (Left . BadRequest . pack)
    (\UserRequest{..} -> Right (UserName username, Password password))
    . eitherDecode
    . encodeUtf8
    . fromStrict

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

