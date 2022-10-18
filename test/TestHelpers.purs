module Test.TestHelpers
  ( mpTest
  , sleep
  ) where

import Prelude

import Control.Monad.Free (Free)
import Effect (Effect)
import Erl.Test.EUnit (TestF, test)
import Pinto.ProcessT (class MonadProcessHandled, class MonadProcessRun, unsafeEvalProcess)

mpTest
  :: forall m mState appMsg parsedMsg
   . MonadProcessRun Effect m mState appMsg parsedMsg
  => MonadProcessHandled m parsedMsg
  => String
  -> m Unit
  -> Free TestF Unit
mpTest desc mpt = test desc $ unsafeEvalProcess mpt

foreign import sleep :: Int -> Effect Unit
