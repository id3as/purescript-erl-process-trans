module Erl.ProcessT.TrapExitT
  ( TrapExitT
  , ExitMessage(..)
  ) where

import Prelude

import Control.Monad.Identity.Trans (IdentityT, runIdentityT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (class HasSelf, self)
import Erl.Process.Raw (setProcessFlagTrapExit, Pid)
import Erl.ProcessT.BusT.Class (class BusM)
import Erl.ProcessT.BusT.MetadataBusT.Class (class MetadataBusM)
import Erl.ProcessT.BusT.StateBusT.Class (class StateBusM)
import Erl.ProcessT.Internal.Types (class MonadProcessHandled, class MonadProcessRun, class MonadProcessTrans, initialise, parseForeign, run)
import Erl.ProcessT.MonitorT.Class (class MonitorM)
import Foreign (Foreign)
import Type.Prelude (Proxy(..))

data ExitMessage = Exit Pid Foreign

foreign import parseTrappedExitFFI :: Foreign -> (Pid -> Foreign -> ExitMessage) -> Maybe ExitMessage

newtype TrapExitT :: forall k. (k -> Type) -> k -> Type
newtype TrapExitT m a = TrapExitT (IdentityT m a)

derive newtype instance Functor m => Functor (TrapExitT m)
derive newtype instance Monad m => Apply (TrapExitT m)
derive newtype instance Monad m => Applicative (TrapExitT m)
derive newtype instance Monad m => Bind (TrapExitT m)
derive newtype instance Monad m => Monad (TrapExitT m)

derive newtype instance MonadEffect m => MonadEffect (TrapExitT m)
derive newtype instance MonadTrans TrapExitT

derive newtype instance MonitorM otherMsg m => MonitorM otherMsg (TrapExitT m)
derive newtype instance BusM otherMsg m => BusM otherMsg (TrapExitT m)
derive newtype instance StateBusM otherMsg m => StateBusM otherMsg (TrapExitT m)
derive newtype instance MetadataBusM otherMsg m => MetadataBusM otherMsg (TrapExitT m)

instance (HasSelf m msg, Monad m) => HasSelf (TrapExitT m) msg where
  self = lift self

instance
  MonadProcessTrans m innerState appMsg innerOutMsg =>
  MonadProcessTrans (TrapExitT m) innerState appMsg (Either ExitMessage innerOutMsg) where
  parseForeign fgn = TrapExitT do
    case parseTrappedExitFFI fgn Exit of
      Just exitMsg ->
        pure $ Just $ Left exitMsg
      Nothing -> do
        (map Right) <$> (lift $ parseForeign fgn)

instance
  MonadProcessRun base m innerState appMsg innerOutMsg =>
  MonadProcessRun base (TrapExitT m) innerState appMsg (Either ExitMessage innerOutMsg) where
  run (TrapExitT mt) is =
    run (runIdentityT mt) is

  initialise _ = do
    void $ liftEffect $ setProcessFlagTrapExit true
    innerState <- initialise (Proxy :: Proxy m)
    pure $ innerState

instance MonadProcessHandled m handledMsg => MonadProcessHandled (TrapExitT m) handledMsg
