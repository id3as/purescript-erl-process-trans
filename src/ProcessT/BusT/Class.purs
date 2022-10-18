module Erl.ProcessT.BusT.Class
  ( Bus
  , bus
  , class BusM
  , subscribe
  , unsubscribe
  ) where

import Prelude

import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Effect.Class (class MonadEffect)

bus :: forall msg name. name -> Bus name msg
bus = Bus

newtype Bus :: Type -> Type -> Type
newtype Bus name msg = Bus name

class MonadEffect m <= BusM busMsgOut m | m -> busMsgOut where
  subscribe
    :: forall name busMsgIn
    . Bus name busMsgIn
    -> (busMsgIn -> busMsgOut)
    -> m Unit

  unsubscribe
    :: forall name busMsgIn
    . Bus name busMsgIn
    -> m Unit

instance BusM msgOut m => BusM msgOut (IdentityT m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)

instance BusM msgOut m => BusM msgOut (StateT s m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)

instance BusM msgOut m => BusM msgOut (ReaderT r m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)

instance (Monoid w, BusM msgOut m) => BusM msgOut (WriterT w m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)
