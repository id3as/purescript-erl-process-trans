module Erl.ProcessT.BusT.StateBusT.Class
  ( class UpdateState
  , updateState
  , Bus
  , BusRef
  , busRef
  , BusMsg(..)
  , class StateBusM
  , subscribe
  , unsubscribe
  ) where

import Prelude

import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)

class UpdateState state msg where
  updateState :: msg -> state -> state

newtype Bus :: Type -> Type -> Type -> Type
newtype Bus name msg state = Bus name

instance Show name => Show (Bus name msg state) where
  show (Bus name) = "Bus " <> show name

newtype BusRef :: Type -> Type -> Type -> Type
newtype BusRef name msg state = BusRef name

derive newtype instance Eq name => Eq (BusRef name msg metadata)
derive newtype instance Ord name => Ord (BusRef name msg metadata)
derive newtype instance Show name => Show (BusRef name msg metadata)

busRef :: forall name msg state. name -> BusRef name msg state
busRef = BusRef

data BusMsg msg state
  = Msg msg
  | State state
  | BusTerminated

derive instance (Eq msg, Eq state) => Eq (BusMsg msg state)
instance (Show msg, Show state) => Show (BusMsg msg state) where
  show (Msg msg) = "Msg " <> show msg
  show (State state) = "State " <> show state
  show BusTerminated = "BusTerminated"

class MonadEffect m <= StateBusM msgOut m | m -> msgOut where
  subscribe
    :: forall name busMsgIn busStateIn
     . BusRef name busMsgIn busStateIn
    -> (BusMsg busMsgIn busStateIn -> msgOut)
    -> m (Maybe busStateIn)

  unsubscribe
    :: forall name busMsgIn busState
     . BusRef name busMsgIn busState
    -> m Unit

instance StateBusM msgOut m => StateBusM msgOut (IdentityT m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)

instance StateBusM msgOut m => StateBusM msgOut (StateT s m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)

instance StateBusM msgOut m => StateBusM msgOut (ReaderT r m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)

instance (Monoid w, StateBusM msgOut m) => StateBusM msgOut (WriterT w m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)
