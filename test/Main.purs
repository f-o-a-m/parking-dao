module Test.Main where

import Prelude

import Chanterelle.Internal.Test (buildTestConfig)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Main (deployScript)
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)
import Node.Process as NP
import ParkingAuthoritySpec (parkingAuthoritySpec)
import SimpleStorageSpec (simpleStorageSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (PROCESS, run', defaultConfig)

-- | TODO: make the options for deploy config env vars
main
  :: forall e.
     Eff ( console :: CONSOLE
         , eth :: ETH
         , avar :: AVAR
         , fs :: FS
         , process :: PROCESS
         , process :: NP.PROCESS
         | e
         ) Unit
main = void <<< launchAff $ do
  testConfig <- buildTestConfig "http://localhost:8545" 60 deployScript
  liftEff $ run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] do
    simpleStorageSpec testConfig
    parkingAuthoritySpec testConfig
