module Erl.ProcessT.BusT.MetadataBusT
  ( MetadataBusT
  , MetadataBusInternal
  , create
  , delete
  , raise
  , updateMetadata
  , module ReExports
  ) where

import Prelude

import Control.Monad.State.Trans (StateT, get, gets, modify_, put, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, Tuple3, tuple2, uncurry2, uncurry3)
import Erl.Kernel.Erlang (monotonicTime)
import Erl.Process (class HasSelf, self)
import Erl.ProcessT.BusT.Class (class BusM)
import Erl.ProcessT.BusT.MetadataBusT.Class (class MetadataBusM, Bus(..), BusMsg(..), BusRef(..), busRef, subscribe, unsubscribe) as ReExports
import Erl.ProcessT.BusT.MetadataBusT.Class (class MetadataBusM, Bus, BusMsg(..), BusRef(..))
import Erl.ProcessT.BusT.StateBusT.Class (class StateBusM)
import Erl.ProcessT.Internal.Types (class MonadProcessHandled, class MonadProcessRun, class MonadProcessTrans, initialise, parseForeign, run)
import Erl.ProcessT.MonitorT.Class (class MonitorM)
import Erl.Types (MonotonicTime)
import Foreign (Foreign)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Generation = Generation (Tuple2 MonotonicTime Int)

derive newtype instance Eq Generation
instance Ord Generation where
  compare = \(Generation tg1) -> tg1 # uncurry2 \t1 g1 ->
    \(Generation tg2) -> tg2 # uncurry2 \t2 g2 ->
      compare t1 t2 <> compare g1 g2

instance Show (Generation) where
  show (Generation gen) = "Generation " <> uncurry2 (const show) gen

data SubscriptionResp metadata = SubscriptionResp Generation metadata

data BusMsgInternal msg metadata
  = DataMsgInternal Generation msg
  | MetadataMsgInternal Generation metadata
  | BusTerminatedInternal Generation

foreign import data BusNameForeign :: Type
foreign import data BusDataForeign :: Type
foreign import data BusMetadataForeign :: Type

newtype MetadataBusInternal msg = MetadataBusInternal
  ( Map BusNameForeign
      { generation :: Maybe Generation
      , monitorRef :: Maybe MetadataBusMonitorRef
      , mapper :: BusMsg BusDataForeign BusMetadataForeign -> msg
      }
  )

newtype MetadataBusT msg m a = MetadataBusT (StateT (MetadataBusInternal msg) m a)

derive newtype instance Functor m => Functor (MetadataBusT msg m)
derive newtype instance Monad m => Apply (MetadataBusT msg m)
derive newtype instance Monad m => Applicative (MetadataBusT msg m)
derive newtype instance Monad m => Bind (MetadataBusT msg m)
derive newtype instance Monad m => Monad (MetadataBusT msg m)

derive newtype instance MonadEffect m => MonadEffect (MetadataBusT msg m)
derive newtype instance MonadTrans (MetadataBusT msg)

derive newtype instance MonitorM otherMsg m => MonitorM otherMsg (MetadataBusT msg m)
derive newtype instance BusM otherMsg m => BusM otherMsg (MetadataBusT msg m)
derive newtype instance StateBusM otherMsg m => StateBusM otherMsg (MetadataBusT msg m)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

create :: forall name msg metadata. BusRef name msg metadata -> metadata -> Effect (Bus name msg metadata)
create busName metadata = do
  t <- monotonicTime
  createImpl busName (Generation (tuple2 t 0)) metadata

foreign import createImpl :: forall name msg metadata. BusRef name msg metadata -> Generation -> metadata -> Effect (Bus name msg metadata)
foreign import deleteImpl :: forall name msg metadata. Bus name msg metadata -> (Generation -> BusMsgInternal msg metadata) -> Effect Unit

delete :: forall name msg metadata. Bus name msg metadata -> Effect Unit
delete busName = deleteImpl busName BusTerminatedInternal

raise :: forall name msg metadata. Bus name msg metadata -> msg -> Effect Unit
raise = raiseImpl

foreign import raiseImpl :: forall name msg metadata. Bus name msg metadata -> msg -> Effect Unit

updateMetadata :: forall name msg metadata. Bus name msg metadata -> metadata -> Effect Unit
updateMetadata = updateMetadataImpl

foreign import updateMetadataImpl :: forall name msg metadata. Bus name msg metadata -> metadata -> Effect Unit

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------
foreign import data MetadataBusPid :: Type
foreign import data MetadataBusMonitorRef :: Type

foreign import subscribeImpl :: forall name msg metadata. BusRef name msg metadata -> Effect (Maybe (Tuple3 Generation metadata MetadataBusMonitorRef))
foreign import unsubscribeImpl :: forall name msg metadata. Maybe MetadataBusMonitorRef -> BusRef name msg metadata -> Effect Unit
foreign import monitorImpl :: MetadataBusPid -> BusNameForeign -> Effect MetadataBusMonitorRef
foreign import demonitorImpl :: MetadataBusMonitorRef -> Effect Unit

instance MonadEffect m => MetadataBusM msgOut (MetadataBusT msgOut m) where
  subscribe
    :: forall name busMsgIn busMetadataIn
     . BusRef name busMsgIn busMetadataIn
    -> (BusMsg busMsgIn busMetadataIn -> msgOut)
    -> MetadataBusT msgOut m (Maybe busMetadataIn)
  subscribe bus mapper =
    MetadataBusT do
      resp <- liftEffect $ subscribeImpl bus
      case resp of
        Nothing -> do
          modify_ \(MetadataBusInternal mm) -> MetadataBusInternal (Map.insert (toBusNameForeign bus) { mapper: toMapperForeign mapper, generation: Nothing, monitorRef: Nothing } mm)
          pure Nothing
        Just genMetadataPidRef -> genMetadataPidRef # uncurry3 \gen metadata ref -> do
          modify_ \(MetadataBusInternal mm) -> MetadataBusInternal (Map.insert (toBusNameForeign bus) { mapper: toMapperForeign mapper, generation: Just gen, monitorRef: Just ref } mm)
          pure $ Just $ metadata

  unsubscribe
    :: forall name busMsgIn busMetadata
     . BusRef name busMsgIn busMetadata
    -> MetadataBusT msgOut m Unit
  unsubscribe bus =
    MetadataBusT do
      maybeRef <- gets \(MetadataBusInternal mm) -> Map.lookup (toBusNameForeign bus) mm >>= _.monitorRef
      modify_ \(MetadataBusInternal mm) -> MetadataBusInternal (Map.delete (toBusNameForeign bus) mm)
      liftEffect $ unsubscribeImpl maybeRef bus

foreign import parseBusMsg :: Foreign -> Maybe (Either (Tuple3 BusNameForeign (BusMsgInternal BusDataForeign BusMetadataForeign) MetadataBusPid) BusNameForeign)

toBusNameForeign :: forall name msg metadata. BusRef name msg metadata -> BusNameForeign
toBusNameForeign = unsafeCoerce

toMapperForeign :: forall msgIn metadataIn outMsg. (BusMsg msgIn metadataIn -> outMsg) -> (BusMsg BusDataForeign BusMetadataForeign -> outMsg)
toMapperForeign = unsafeCoerce

toBusMsg :: forall msg metadata. Maybe Generation -> BusMsgInternal msg metadata -> Maybe { generation :: Generation, message :: BusMsg msg metadata }
toBusMsg currentGeneration busMsgInternal =
  case busMsgInternal of
    MetadataMsgInternal g metadata ->
      lifecycleGeneration g (MetadataMsg metadata)

    BusTerminatedInternal g ->
      lifecycleGeneration g BusTerminated

    DataMsgInternal g msg ->
      case currentGeneration of
        Just c
          -- We do not expect generation to be incremented for DataMsgInternal
          | g >= c ->
              Just { generation: g, message: DataMsg msg }
          | otherwise ->
              Nothing
        Nothing ->
          Nothing
  where
  lifecycleGeneration g message =
    case currentGeneration of
      Nothing ->
        Just { generation: g, message }
      Just c
        | g > c ->
            Just { generation: g, message }
        | otherwise ->
            Nothing

instance
  MonadProcessTrans m innerMetadata appMsg innerOutMsg =>
  MonadProcessTrans (MetadataBusT msgOut m) (Tuple (MetadataBusInternal msgOut) innerMetadata) appMsg (Either msgOut innerOutMsg) where
  parseForeign fgn = MetadataBusT do
    case parseBusMsg fgn of
      Just (Left busNameMsg) ->
        busNameMsg # uncurry3 \busName busMsgInternal busPid -> do
          MetadataBusInternal mtMetadata <- get
          case Map.lookup busName mtMetadata of
            Nothing -> do
              pure Nothing
            Just { generation, mapper, monitorRef: maybeMonitorRef } -> do
              case toBusMsg generation busMsgInternal of
                Nothing -> do
                  pure Nothing
                Just { generation: _, message: busMsg@BusTerminated } -> do
                  case maybeMonitorRef of
                    Just monitorRef -> liftEffect $ demonitorImpl monitorRef
                    Nothing -> pure unit
                  put $ MetadataBusInternal $ Map.delete busName mtMetadata
                  pure $ Just $ Left $ mapper busMsg
                Just { generation: newGeneration, message: busMsg } -> do
                  monitorRef <- case maybeMonitorRef of
                    Just monitorRef -> pure monitorRef
                    Nothing -> liftEffect $ monitorImpl busPid busName
                  put $ MetadataBusInternal $ Map.insert busName { generation: Just newGeneration, mapper, monitorRef: Just monitorRef } mtMetadata
                  pure $ Just $ Left $ mapper busMsg
      Just (Right busName) -> do
        MetadataBusInternal mtMetadata <- get
        case Map.lookup busName mtMetadata of
          Nothing -> do
            pure Nothing
          Just { mapper, monitorRef } -> do
            put $ MetadataBusInternal $ Map.delete busName mtMetadata
            liftEffect $ unsubscribeImpl monitorRef (BusRef busName)
            pure $ Just $ Left $ mapper BusTerminated
      Nothing -> do
        (map Right) <$> (lift $ parseForeign fgn)

instance
  MonadProcessRun base m innerMetadata appMsg innerOutMsg =>
  MonadProcessRun base (MetadataBusT msgOut m) (Tuple (MetadataBusInternal msgOut) innerMetadata) appMsg (Either msgOut innerOutMsg) where
  run (MetadataBusT mt) (Tuple mtMetadata is) = do
    (Tuple (Tuple res newMtMetadata) newIs) <- run (runStateT mt mtMetadata) is
    pure $ Tuple res $ Tuple newMtMetadata newIs

  initialise _ = do
    innerMetadata <- initialise (Proxy :: Proxy m)
    pure $ Tuple (MetadataBusInternal Map.empty) innerMetadata

instance MonadProcessHandled m handledMsg => MonadProcessHandled (MetadataBusT msgOut m) handledMsg

instance (HasSelf m msg, Monad m) => HasSelf (MetadataBusT msgOut m) msg where
  self = lift self
