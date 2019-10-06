module Persist
  ( Persist(..)
  , Persistence(..)
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Has (Has(..))

import Types

class Monad m => Persist m where
  persistUser :: UserName -> Password -> m UserId
  deleteUser :: UserId -> m Deletion

data Persistence m = Persistence
  { doPersistUser :: UserName -> Password -> m UserId
  , doDeleteUser :: UserId -> m Deletion
  }

instance
  ( Has (Persistence m) r
  , Monad m
  ) => Persist (ReaderT r m) where
  persistUser user pass =
    asks getter >>= \(Persistence persist _) -> lift $ persist user pass
  deleteUser userId =
    asks getter >>= \(Persistence _ get) -> lift $ get userId
