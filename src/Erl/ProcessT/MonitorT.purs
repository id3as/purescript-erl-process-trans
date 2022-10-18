module Erl.ProcessT.MonitorT
  ( MonitorMap
  , MonitorT
  , spawnLinkMonitor
  , spawnLinkMonitor'
  , spawnMonitor
  , spawnMonitor'
  , module ReExports
  ) where

import Prelude

import Control.Monad.State.Trans (StateT, get, modify_, put, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process (class HasSelf, Process, self)
import Erl.Process.Raw (class HasPid, getPid)
import Erl.Process.Raw as Raw
import Erl.ProcessT (spawn, spawn', spawnLink, spawnLink')
import Erl.ProcessT.BusT.Class (class BusM)
import Erl.ProcessT.BusT.MetadataBusT.Class (class MetadataBusM)
import Erl.ProcessT.BusT.StateBusT.Class (class StateBusM)
import Erl.ProcessT.Internal.Types (class MonadProcessHandled, class MonadProcessRun, class MonadProcessTrans, initialise, parseForeign, run)
import Erl.ProcessT.MonitorT.Class (class MonitorM, MonitorInfo, MonitorMsg(..), MonitorObject, MonitorRef, MonitorType(..), demonitor, monitor) as ReExports
import Erl.ProcessT.MonitorT.Class (class MonitorM, MonitorMsg(..), MonitorRef, monitor)
import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (Proxy(..))

newtype MonitorT monitorMsg m a = MonitorT (StateT (MonitorMap monitorMsg) m a)

derive newtype instance Functor m => Functor (MonitorT monitorMsg m)
derive newtype instance Monad m => Apply (MonitorT monitorMsg m)
derive newtype instance Monad m => Applicative (MonitorT monitorMsg m)
derive newtype instance Monad m => Bind (MonitorT monitorMsg m)
derive newtype instance Monad m => Monad (MonitorT monitorMsg m)

derive newtype instance MonadEffect m => MonadEffect (MonitorT monitorMsg m)
derive newtype instance MonadTrans (MonitorT monitorMsg)

derive newtype instance BusM otherMsg m => BusM otherMsg (MonitorT msg m)
derive newtype instance StateBusM otherMsg m => StateBusM otherMsg (MonitorT msg m)
derive newtype instance MetadataBusM otherMsg m => MetadataBusM otherMsg (MonitorT msg m)

instance (HasSelf m msg, Monad m) => HasSelf (MonitorT monitorMsg m) msg where
  self = lift self

type MonitorMap msg = Map MonitorRef (MonitorMsg -> msg)

foreign import monitorImpl :: Raw.Pid -> Effect MonitorRef
foreign import demonitorImpl :: MonitorRef -> Effect Unit
foreign import parseMonitorMsg :: Foreign -> Maybe MonitorMsg

-- instance
--   (HasTypedPid m msg) =>
--   HasTypedPid (MonitorT monitorMsg m) msg where
--     getTypedPid _ = getTypedPid (Proxy :: Proxy m)

instance
  MonadProcessTrans m innerState appMsg innerOutMsg =>
  MonadProcessTrans (MonitorT monitorMsg m) (Tuple (MonitorMap monitorMsg) innerState) appMsg (Either monitorMsg innerOutMsg) where
  parseForeign fgn = MonitorT do
    case parseMonitorMsg fgn of
      Just down@(Down ref _ _ _) -> do
        mtState <- get
        case Map.lookup ref mtState of
          Nothing ->
            unsafeCrashWith "Down from unknown monitor"
          Just mapper -> do
            put $ Map.delete ref mtState
            pure $ Just $ Left $ mapper down
      Nothing -> do
        (map Right) <$> (lift $ parseForeign fgn)

instance
  MonadProcessRun base m innerState appMsg innerOutMsg =>
  MonadProcessRun base (MonitorT monitorMsg m) (Tuple (MonitorMap monitorMsg) innerState) appMsg (Either monitorMsg innerOutMsg) where
  run (MonitorT mt) (Tuple mtState is) = do
    (Tuple (Tuple res newMtState) newIs) <- run (runStateT mt mtState) is
    pure $ Tuple res $ Tuple newMtState newIs

  initialise _ = do
    innerState <- initialise (Proxy :: Proxy m)
    pure $ Tuple Map.empty innerState

instance MonadProcessHandled m handledMsg => MonadProcessHandled (MonitorT monitorMsg m) handledMsg

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------
instance MonadEffect m => MonitorM monitorMsg (MonitorT monitorMsg m) where
  monitor
    :: forall pid
    . HasPid pid
    => pid
    -> (MonitorMsg -> monitorMsg)
    -> MonitorT monitorMsg m MonitorRef
  monitor pid mapper = do
    MonitorT do
      ref <- liftEffect $ monitorImpl $ getPid pid
      modify_ \mm -> Map.insert ref mapper mm
      pure ref

  demonitor
    :: MonitorRef
    -> MonitorT monitorMsg m Unit
  demonitor ref = do
    MonitorT do
      liftEffect $ demonitorImpl ref
      modify_ \mm -> Map.delete ref mm

spawnMonitor
  :: forall m mState msg outMsg m2 monitorMsg
   . MonadProcessHandled m outMsg
  => MonadProcessRun Effect m mState msg outMsg
  => MonitorM monitorMsg m2
  => m Unit
  -> (MonitorMsg -> monitorMsg)
  -> m2 (Process msg)
spawnMonitor = doSpawnMonitor spawn

spawnLinkMonitor
  :: forall m mState msg outMsg m2 monitorMsg
   . MonadProcessHandled m outMsg
  => MonadProcessRun Effect m mState msg outMsg
  => MonitorM monitorMsg m2
  => m Unit
  -> (MonitorMsg -> monitorMsg)
  -> m2 (Process msg)
spawnLinkMonitor = doSpawnMonitor spawnLink

spawnMonitor'
  :: forall base m mState msg outMsg m2 monitorMsg
   . MonadProcessHandled m outMsg
  => MonadProcessRun base m mState msg outMsg
  => MonitorM monitorMsg m2
  => (base ~> Effect)
  -> m Unit
  -> (MonitorMsg -> monitorMsg)
  -> m2 (Process msg)
spawnMonitor' runBase = doSpawnMonitor (spawn' runBase)

spawnLinkMonitor'
  :: forall base m mState msg outMsg m2 monitorMsg
   . MonadProcessHandled m outMsg
  => MonadProcessRun base m mState msg outMsg
  => MonitorM monitorMsg m2
  => (base ~> Effect)
  -> m Unit
  -> (MonitorMsg -> monitorMsg)
  -> m2 (Process msg)
spawnLinkMonitor' runBase = doSpawnMonitor (spawnLink' runBase)

-- TODO - consider modelling the erlang capabilities around  alias, reply_demonitor, user defined tags etc

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------
doSpawnMonitor
  :: forall m msg m2 monitorMsg
   . MonitorM monitorMsg m2
  => (m -> Effect (Process msg))
  -> m
  -> (MonitorMsg -> monitorMsg)
  -> m2 (Process msg)
doSpawnMonitor spawner m mapper = do
  pid <- liftEffect $ spawner m
  void $ monitor pid mapper
  pure pid
