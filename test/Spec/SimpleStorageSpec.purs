module SimpleStorageSpec (simpleStorageSpec) where

import Prelude

import Chanterelle.Internal.Test (TestConfig)
import Data.Array ((!!))
import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3 (Address, ETH, EventAction(..), _from, _gas, _to, defaultTransactionOptions, embed, event, eventFilter, runWeb3, uIntNFromBigNumber)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafePartial, unsafePartialBecause)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))

simpleStorageSpec
  :: forall e r.
     TestConfig (simpleStorage :: Address | r)
  -> Spec ( fs :: FS
          , eth :: ETH
          , avar :: AVAR
          , console :: CONSOLE
          | e
          ) Unit
simpleStorageSpec {provider, accounts, simpleStorage} = do

  describe "Setting the value of a SimpleStorage Contract" do
    it "can set the value of simple storage" $ do
      let primaryAccount = unsafePartialBecause "Accounts list has at least one account" $ fromJust (accounts !! 0)
      var <- makeEmptyVar
      let n = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ 42
          txOptions = defaultTransactionOptions # _from ?~ primaryAccount
                                                # _to ?~ simpleStorage
                                                # _gas ?~ embed 90000
      hx <- runWeb3 provider $ SimpleStorage.setCount txOptions {_count: n}
      liftEff <<< log $ "setCount tx hash: " <> show hx
      let filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage
      _ <- liftAff $ runWeb3 provider $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          liftEff $ log $ "Received Event: " <> show e
          _ <- liftAff $ putVar cs._count var
          pure TerminateEvent
      val <- takeVar var
      Just val `shouldEqual` Just n
