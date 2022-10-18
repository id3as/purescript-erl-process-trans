module Test.Main where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Kernel.Application (ensureAllStarted)
import Erl.Test.EUnit (runTests)
import Test.BusT (testBusT)
import Test.MetadataBusT (testMetadataBusT)
import Test.MonitorT (testMonitorT)
import Test.StateBusT (testStateBusT)
import Test.StateMetadataBusT (testStateMetadataBusT)
import Test.TrapExitT (testTrapExitT)

foreign import filterSasl :: Effect Unit

main :: Effect Unit
main = do
  filterSasl
  void $ ensureAllStarted $ atom "gproc"
  void $ runTests do
    testMonitorT
    testTrapExitT
    testBusT
    testStateBusT
    testMetadataBusT
    testStateMetadataBusT
