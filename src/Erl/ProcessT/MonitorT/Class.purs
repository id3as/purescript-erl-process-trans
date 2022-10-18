module Erl.ProcessT.MonitorT.Class where

import Prelude

import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Effect.Class (class MonadEffect)
import Erl.Process.Raw (class HasPid)
import Foreign (Foreign)


type MonitorObject = Foreign

-- | The 'reason' for the monitor being invoked, if this needs unpacking
-- | then FFI will need to be written
type MonitorInfo = Foreign

-- | The type of monitor this message is being sent on behalf
data MonitorType
  = Process
  | Port

data MonitorMsg = Down MonitorRef MonitorType MonitorObject MonitorInfo

-- | Reference to a monitor, used to stop the monitor once it is started
foreign import data MonitorRef :: Type

class MonadEffect m <= MonitorM monitorMsg m | m -> monitorMsg where
  monitor
    :: forall pid
    . HasPid pid
    => pid
    -> (MonitorMsg -> monitorMsg)
    -> m MonitorRef

  demonitor
    :: MonitorRef
    -> m Unit

instance MonitorM msgOut m => MonitorM msgOut (IdentityT m) where
  monitor ref mapper = lift (monitor ref mapper)
  demonitor ref = lift (demonitor ref)

instance MonitorM msgOut m => MonitorM msgOut (StateT s m) where
  monitor ref mapper = lift (monitor ref mapper)
  demonitor ref = lift (demonitor ref)

instance MonitorM msgOut m => MonitorM msgOut (ReaderT r m) where
  monitor ref mapper = lift (monitor ref mapper)
  demonitor ref = lift (demonitor ref)

instance (Monoid w, MonitorM msgOut m) => MonitorM msgOut (WriterT w m) where
  monitor ref mapper = lift (monitor ref mapper)
  demonitor ref = lift (demonitor ref)
