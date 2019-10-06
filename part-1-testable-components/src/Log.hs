module Log
  ( Loggable(..)
  , Log(..)
  , Logger(..)
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader.Class (asks)
import Data.Has (Has(..))
import Data.Text (Text)
import GHC.Stack (HasCallStack)

class Loggable a where
  fromLoggable :: a -> Text

class Monad m => Log m where
  logLn :: HasCallStack => Loggable a => a -> m ()

data Logger m = Logger
  { dologLn :: HasCallStack => Text -> m ()
  }

instance
  ( Has (Logger m) r
  , Monad m
  ) => Log (ReaderT r m) where
  logLn a =
    asks getter >>= \(Logger doLog) -> lift . doLog . fromLoggable $ a

instance Loggable Text where
  fromLoggable = id
