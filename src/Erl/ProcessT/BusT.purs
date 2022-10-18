module Erl.ProcessT.BusT
  ( BusMap
  , BusMsgForeign
  , BusNameForeign
  , BusT
  , raise
  , module ReExports
  ) where

import Prelude

import Control.Monad.State.Trans (StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, fst, snd)
import Erl.Process (class HasSelf, self)
import Erl.ProcessT.BusT.Class (class BusM, Bus)
import Erl.ProcessT.BusT.Class (class BusM, Bus, bus, subscribe, unsubscribe) as ReExports
import Erl.ProcessT.BusT.MetadataBusT.Class (class MetadataBusM)
import Erl.ProcessT.BusT.StateBusT.Class (class StateBusM)
import Erl.ProcessT.Internal.Types (class MonadProcessHandled, class MonadProcessRun, class MonadProcessTrans, initialise, parseForeign, run)
import Erl.ProcessT.MonitorT.Class (class MonitorM)
import Foreign (Foreign)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data BusNameForeign :: Type
foreign import data BusMsgForeign :: Type

type BusMap msg = Map BusNameForeign (BusMsgForeign -> msg)

newtype BusT busMsg m a = BusT (StateT (BusMap busMsg) m a)

derive newtype instance Functor m => Functor (BusT busMsg m)
derive newtype instance Monad m => Apply (BusT busMsg m)
derive newtype instance Monad m => Applicative (BusT busMsg m)
derive newtype instance Monad m => Bind (BusT busMsg m)
derive newtype instance Monad m => Monad (BusT busMsg m)

derive newtype instance MonadEffect m => MonadEffect (BusT busMsg m)
derive newtype instance MonadTrans (BusT busMsg)

derive newtype instance MonitorM otherMsg m => MonitorM otherMsg (BusT msg m)
derive newtype instance StateBusM otherMsg m => StateBusM otherMsg (BusT msg m)
derive newtype instance MetadataBusM otherMsg m => MetadataBusM otherMsg (BusT msg m)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------
foreign import raise :: forall name msg. Bus name msg -> msg -> Effect Unit

instance MonadEffect m => BusM busMsgOut (BusT busMsgOut m) where
  subscribe
    :: forall name busMsgIn
     . Bus name busMsgIn
    -> (busMsgIn -> busMsgOut)
    -> BusT busMsgOut m Unit
  subscribe bus mapper =
    BusT do
      modify_ \mm -> Map.insert (toBusNameForeign bus) (toMapperForeign mapper) mm
      liftEffect $ subscribeImpl bus

  unsubscribe
    :: forall name busMsgIn
     . Bus name busMsgIn
    -> BusT busMsgOut m Unit
  unsubscribe bus =
    BusT do
      modify_ \mm -> Map.delete (toBusNameForeign bus) mm
      liftEffect $ unsubscribeImpl bus

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------
foreign import subscribeImpl :: forall name msg. Bus name msg -> Effect Unit
foreign import unsubscribeImpl :: forall name msg. Bus name msg -> Effect Unit
foreign import parseBusMsg :: Foreign -> Maybe (Tuple2 BusNameForeign BusMsgForeign)

toBusNameForeign :: forall name msg. Bus name msg -> BusNameForeign
toBusNameForeign = unsafeCoerce

toMapperForeign :: forall inMsg outMsg. (inMsg -> outMsg) -> (BusMsgForeign -> outMsg)
toMapperForeign = unsafeCoerce

instance
  MonadProcessTrans m innerState appMsg innerOutMsg =>
  MonadProcessTrans (BusT busMsg m) (Tuple (BusMap busMsg) innerState) appMsg (Either busMsg innerOutMsg) where
  parseForeign fgn = BusT do
    case parseBusMsg fgn of
      Just busNameMsg -> do
        mtState <- get
        case Map.lookup (fst busNameMsg) mtState of
          Nothing ->
            pure Nothing
          Just mapper -> do
            pure $ Just $ Left $ mapper $ snd busNameMsg
      Nothing -> do
        (map Right) <$> (lift $ parseForeign fgn)

instance
  MonadProcessRun base m innerState appMsg innerOutMsg =>
  MonadProcessRun base (BusT busMsg m) (Tuple (BusMap busMsg) innerState) appMsg (Either busMsg innerOutMsg) where
  run (BusT mt) (Tuple mtState is) = do
    (Tuple (Tuple res newMtState) newIs) <- run (runStateT mt mtState) is
    pure $ Tuple res $ Tuple newMtState newIs

  initialise _ = do
    innerState <- initialise (Proxy :: Proxy m)
    pure $ Tuple Map.empty innerState

instance MonadProcessHandled m handledMsg => MonadProcessHandled (BusT busMsg m) handledMsg

instance (HasSelf m msg, Monad m) => HasSelf (BusT busMsgOut m) msg where
  self = lift self
