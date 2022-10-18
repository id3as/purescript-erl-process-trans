module Erl.ProcessT.BusT.MetadataBusT.Class where

import Prelude

import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)

newtype Bus :: Type -> Type -> Type -> Type
newtype Bus name msg metadata = Bus name

instance Show name => Show (Bus name msg metadata) where
  show (Bus name) = "Bus " <> show name

newtype BusRef :: Type -> Type -> Type -> Type
newtype BusRef name msg metadata = BusRef name

derive newtype instance Eq name => Eq (BusRef name msg metadata)
derive newtype instance Ord name => Ord (BusRef name msg metadata)
derive newtype instance Show name => Show (BusRef name msg metadata)

busRef :: forall name msg metadata. name -> BusRef name msg metadata
busRef = BusRef

data BusMsg msg metadata
  = DataMsg msg
  | MetadataMsg metadata
  | BusTerminated

derive instance (Eq msg, Eq metadata) => Eq (BusMsg msg metadata)
instance (Show msg, Show metadata) => Show (BusMsg msg metadata) where
  show (DataMsg msg) = "DataMsg " <> show msg
  show (MetadataMsg metadata) = "MetadataMsg " <> show metadata
  show BusTerminated = "BusTerminated"

class MonadEffect m <= MetadataBusM msgOut m | m -> msgOut where
  subscribe
    :: forall name busMsgIn busMetadataIn
     . BusRef name busMsgIn busMetadataIn
    -> (BusMsg busMsgIn busMetadataIn -> msgOut)
    -> m (Maybe busMetadataIn)

  unsubscribe
    :: forall name busMsgIn busMetadata
     . BusRef name busMsgIn busMetadata
    -> m Unit

instance MetadataBusM msgOut m => MetadataBusM msgOut (IdentityT m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)

instance MetadataBusM msgOut m => MetadataBusM msgOut (StateT s m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)

instance MetadataBusM msgOut m => MetadataBusM msgOut (ReaderT r m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)

instance (Monoid w, MetadataBusM msgOut m) => MetadataBusM msgOut (WriterT w m) where
  subscribe ref mapper = lift (subscribe ref mapper)
  unsubscribe ref = lift (unsubscribe ref)
