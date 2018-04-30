module ParkingAuthoritySpec (parkingAuthoritySpec) where


import Prelude

import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Chanterelle.Test (TestConfig, assertWeb3, takeEvent)
import Contracts.ParkingAnchor as ParkingAnchor
import Contracts.ParkingAuthority as PA
import Contracts.User as User
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Lens.Setter ((?~))
import Data.Maybe (Maybe(..))
import Data.String (take)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Web3 (TransactionStatus(..))
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (BytesN, fromByteString)
import Network.Ethereum.Web3.Solidity.Sizes (S32, S4, S8, s32, s4, s8)
import Network.Ethereum.Web3.Types (Address, BigNumber, ChainCursor(..), TransactionReceipt(..), ETH, Web3, _from, _gas, _to, _value, defaultTransactionOptions, embed, fromWei)
import Node.FS.Aff (FS)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))


parkingAuthoritySpec
  :: forall e r.
     TestConfig (foamCSR :: Address, parkingAuthority :: Address | r)
  -> Spec ( fs :: FS
          , eth :: ETH
          , avar :: AVAR
          , console :: CONSOLE
          | e
          ) Unit
parkingAuthoritySpec testCfg@{provider, accounts, foamCSR, parkingAuthority} = do

  describe "Testing basic functionality of the parking authority" do
    it "has the correct foamCSR contract" do
      let txOpts = defaultTransactionOptions # _to ?~ parkingAuthority
      csr <- assertWeb3 provider $ PA.parkingCSR txOpts Latest
      csr `shouldEqual` Right foamCSR

  describe "App Flow" do
    it "can register a user and that user is owned by the right account" $ assertWeb3 provider do
      void $ createUser testCfg 1


    it "can create a user and that user can request more zones from the authority" $ assertWeb3 provider do
      {user, owner} <- createUser testCfg 1
      let
        zone :: BytesN S4
        zone = case fromByteString s4 =<< BS.fromString "01234567" BS.Hex of
          Just x -> x
          Nothing -> unsafeCrashWith "zone should result in valid BytesN D4"
        txOpts = defaultTransactionOptions # _from ?~ owner
                                           # _gas ?~ bigGasLimit
                                           # _to ?~ user
      Tuple _ e@(User.ZoneGranted {zone: eventZone}) <- takeEvent (Proxy :: Proxy User.ZoneGranted) user
        $ User.requestZone txOpts { _zone: zone }
      liftAff <<< log $ "Received Event: " <> show e
      liftAff $ zone `shouldEqual` eventZone

    it "can create an anchor, and that anchor is owned by the right account" $ assertWeb3 provider do
      let _anchorId = case fromByteString s32 $ keccak256 "I'm an anchor!" of
            Just x -> x
            Nothing -> unsafeCrashWith "anchorId should result in valid BytesN 32"
          _geohash = case fromByteString s8 =<< BS.fromString ("0123456701234567") BS.Hex of
            Just x -> x
            Nothing -> unsafeCrashWith "geohash should result in valid BytesN 8"
      void $ createParkingAnchor testCfg 2 {_geohash, _anchorId}

    it "can create a user and an anchor, the user requests permission at the anchor, then parks there, but not another zone" $ assertWeb3 provider do
      userResult <- createUser testCfg 1
      let _anchorId = case fromByteString s32 $ keccak256 "I'm an anchor!" of
            Just x -> x
            Nothing -> unsafeCrashWith "anchorId should result in valid BytesN 32"
          geohashString = "0123456701234567"
          _geohash = case fromByteString s8 =<< BS.fromString geohashString BS.Hex of
            Just x -> x
            Nothing -> unsafeCrashWith "geohash should result in valid BytesN 8"
      parkingAnchorResult <- createParkingAnchor testCfg 2 {_geohash, _anchorId}
      let
        zoneStr = take 8 geohashString
        zone = case fromByteString s4 =<< BS.fromString zoneStr BS.Hex of
          Just x -> x
          Nothing -> unsafeCrashWith "zone should result in valid BytesN D4"
        txOpts = defaultTransactionOptions # _from ?~ userResult.owner
                                           # _gas ?~ bigGasLimit
                                           # _to ?~ userResult.user
      (Tuple _ e) <- takeEvent (Proxy :: Proxy User.ZoneGranted) userResult.user $ User.requestZone txOpts { _zone: zone }
      liftAff <<< log $ "Received Event: " <> show e
      let parkingReqOpts = txOpts # _value ?~ (fromWei $ embed 1)
      Tuple _ (User.CheckIn {user, anchor}) <- takeEvent (Proxy :: Proxy User.CheckIn) userResult.user $
        User.payForParking parkingReqOpts {_anchor: parkingAnchorResult.anchor}
      liftAff $ user `shouldEqual` userResult.user
      liftAff $ anchor `shouldEqual` parkingAnchorResult.anchor

      badUserResult <- createUser testCfg 3
      let
        badZone :: BytesN S4
        badZone = case fromByteString s4 =<< BS.fromString "00000000" BS.Hex of
          Just x -> x
          Nothing -> unsafeCrashWith "zone should result in valid BytesN D4"

        badTxOpts = defaultTransactionOptions # _from ?~ badUserResult.owner
                                              # _gas ?~ bigGasLimit
                                              # _to ?~ badUserResult.user
                                              # _value ?~ fromWei (embed 1)
      badHash <- User.payForParking badTxOpts {_anchor: parkingAnchorResult.anchor}
      (TransactionReceipt txReceipt) <- liftAff $ pollTransactionReceipt badHash provider
      liftAff $ txReceipt.status `shouldEqual` Failed

--------------------------------------------------------------------------------
-- | SetupTests
--------------------------------------------------------------------------------
-- Setup tests are like sub-tests, they create and account (either user or parking) and
-- return all the necessary information about that account to use it in another test

createUser
  :: forall eff r.
     TestConfig (parkingAuthority :: Address | r)
  -> Int
  -- ^ the index of the account to use for transactions
  -> Web3 (fs :: FS, avar :: AVAR, console :: CONSOLE | eff) {owner :: Address, user :: Address}
createUser testCfg@{accounts} accountIndex = do
  let
    account = case accounts !! accountIndex of
      Just x -> x
      Nothing -> unsafeCrashWith $ "no index " <> show accountIndex <> "in accounts"
  {owner, user} <- registerUser account testCfg
  let txOpts = defaultTransactionOptions # _from ?~ account
                                         # _to ?~ user
  actualOwner <- User.owner txOpts Latest <#> case _ of
    Right x -> x
    Left err -> unsafeCrashWith $ "expected Right in `User.owner`, got error" <> show err
  liftAff $ owner `shouldEqual` actualOwner
  liftAff $ owner `shouldEqual` account
  pure {owner, user}

createParkingAnchor
  :: forall eff r.
     TestConfig (parkingAuthority :: Address | r)
  -> Int
  -- ^ the index for the account to use for transactions
  -> {_geohash :: BytesN S8, _anchorId :: BytesN S32 }
  -> Web3 (fs :: FS, console :: CONSOLE, avar :: AVAR | eff) {owner :: Address, anchor :: Address, anchorId :: BytesN S32 , geohash :: BytesN S8}
createParkingAnchor testConfig accountIndex args = do
  accounts <- eth_getAccounts
  let
    account = case accounts !! accountIndex of
      Just x -> x
      Nothing -> unsafeCrashWith $ "no index " <> show accountIndex <> "in accounts"
  res@{owner, anchor, geohash, anchorId} <- registerAnchor account args testConfig
  let txOpts = defaultTransactionOptions # _from ?~ account
                                         # _to ?~ anchor
  actualOwner <- ParkingAnchor.owner txOpts Latest <#> case _ of
    Right x -> x
    Left err -> unsafeCrashWith $ "expected Right in `ParkingAnchor.owner`, got error" <> show err

  liftAff $ owner `shouldEqual` actualOwner
  liftAff $ owner `shouldEqual` account
  liftAff $ geohash `shouldEqual` args._geohash
  liftAff $ anchorId `shouldEqual` args._anchorId
  pure res


registerUser
  :: forall eff r.
     Address
  -- ^ from address
  -> TestConfig (parkingAuthority :: Address | r)
  -> Web3 (fs :: FS, avar :: AVAR, console :: CONSOLE | eff) {owner :: Address, user :: Address}
registerUser fromAccount {provider, parkingAuthority} = do
  let txOpts = defaultTransactionOptions # _from ?~ fromAccount
                                         # _gas ?~ bigGasLimit
                                         # _to ?~ parkingAuthority
  Tuple _ (PA.RegisterParkingUser ev) <- takeEvent (Proxy :: Proxy PA.RegisterParkingUser) parkingAuthority do
      txHash <- PA.registerUser txOpts
      liftAff <<< log $ "Registered User " <> show fromAccount <> ", Transaction Hash:" <> show txHash
  pure ev

registerAnchor
  :: forall eff r.
     Address
  -- ^ from address
  -> {_geohash :: BytesN S8, _anchorId :: BytesN S32 }
  -> TestConfig (parkingAuthority :: Address | r)
  -> Web3 (fs :: FS, console :: CONSOLE, avar :: AVAR | eff) {owner :: Address, anchor :: Address, anchorId :: BytesN S32 , geohash :: BytesN S8 }
registerAnchor fromAccount args {provider, parkingAuthority} = do
  let txOpts = defaultTransactionOptions # _from ?~ fromAccount
                                         # _gas ?~ bigGasLimit
                                         # _to ?~ parkingAuthority
  Tuple _ (PA.RegisteredParkingAnchor ev) <- takeEvent (Proxy :: Proxy PA.RegisteredParkingAnchor) parkingAuthority do
    txHash <- PA.registerParkingAnchor txOpts args
    liftAff <<< log $ "Registered Anchor " <> show fromAccount <> ", Transaction Hash:" <> show txHash
  pure ev

bigGasLimit :: BigNumber
bigGasLimit = case parseBigNumber decimal "4712388" of
  Just x -> x
  Nothing -> unsafeCrashWith "expected to get big number from 4712388 but it failed"
