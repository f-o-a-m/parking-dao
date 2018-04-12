module Test.Main where

import Prelude

import Chanterelle.Internal.Test (TestConfig)
import Chanterelle.Internal.Types (DeployConfig(..), DeployError(..), logDeployError, runDeployM)
import Chanterelle.Internal.Utils (makeDeployConfig)
import Control.Monad.Aff (Aff, launchAff, liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Main (DeployResults, deployScript)
import Network.Ethereum.Web3 (ETH, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts)
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
  testConfig <- testDeployment "http://localhost:8545" 60
  liftEff $ run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] do
    simpleStorageSpec testConfig
    parkingAuthoritySpec testConfig

testDeployment
  :: forall eff.
     String
  -> Int
  -> Aff (console :: CONSOLE, eth :: ETH, fs :: FS | eff) (TestConfig DeployResults)
testDeployment url timeout = do
  edeployConfig <- unsafeCoerceAff <<< runExceptT $ makeDeployConfig url timeout
  case edeployConfig of
    Left err -> logDeployError err *> (liftEff' $ throw "Couldn't make deploy config for tests!")
    Right deployConfig@(DeployConfig {provider}) -> do
      eDeployResults <- flip runDeployM deployConfig $ do
        eaccounts <- liftAff $ runWeb3 provider eth_getAccounts
        accounts <- either (throwError <<< ConfigurationError <<< show) pure eaccounts
        results <- deployScript
        pure $ Tuple accounts results
      case eDeployResults of
        Left err -> logDeployError err *> (liftEff' $ throw "Error during deployment!")
        Right (Tuple accounts results) -> pure { accounts
                                               , provider
                                               , foamCSR: results.foamCSR
                                               , simpleStorage: results.simpleStorage
                                               , parkingAuthority: results.parkingAuthority
                                               }
