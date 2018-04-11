--------------------------------------------------------------------------------
-- | FoamCSR
--------------------------------------------------------------------------------

module Contracts.FoamCSR where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D8, Tuple1(..), Tuple2(..), Tuple3, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | RegisterFn
--------------------------------------------------------------------------------


type RegisterFn = Tagged (SProxy "register(address)") (Tuple1 Address)

register :: forall e. TransactionOptions NoPay -> { newCsc :: Address } -> Web3 e HexString
register x0 r = uncurryFields  r $ register' x0
   where
    register' :: TransactionOptions NoPay -> Tagged (SProxy "newCsc") Address -> Web3 e HexString
    register' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: RegisterFn)

--------------------------------------------------------------------------------
-- | RegistryFn
--------------------------------------------------------------------------------


type RegistryFn = Tagged (SProxy "registry(bytes12)") (Tuple1 (BytesN (D1 :& D2)))

registry :: forall e. TransactionOptions NoPay -> ChainCursor -> { csc :: (BytesN (D1 :& D2)) } -> Web3 e (Either CallError Address)
registry x0 cm r = uncurryFields  r $ registry' x0 cm
   where
    registry' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "csc") (BytesN (D1 :& D2)) -> Web3 e (Either CallError Address)
    registry' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: RegistryFn)

--------------------------------------------------------------------------------
-- | ComputeCSCFn
--------------------------------------------------------------------------------


type ComputeCSCFn = Tagged (SProxy "computeCSC(bytes8,address)") (Tuple2 (BytesN D8) Address)

computeCSC :: forall e. TransactionOptions NoPay -> ChainCursor -> { geohash_arg :: (BytesN D8), addr :: Address } -> Web3 e (Either CallError (BytesN (D1 :& D2)))
computeCSC x0 cm r = uncurryFields  r $ computeCSC' x0 cm
   where
    computeCSC' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "geohash_arg") (BytesN D8) -> Tagged (SProxy "addr") Address -> Web3 e (Either CallError (BytesN (D1 :& D2)))
    computeCSC' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: ComputeCSCFn)

--------------------------------------------------------------------------------
-- | RegisterCSC
--------------------------------------------------------------------------------


newtype RegisterCSC = RegisterCSC {callerAddress :: Address,csc :: (BytesN (D1 :& D2)),cscAddress :: Address,cscGeohash :: (BytesN D8)}

derive instance newtypeRegisterCSC :: Newtype RegisterCSC _

instance eventFilterRegisterCSC :: EventFilter RegisterCSC where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e61569568ccda2c09552ec0288a98a921c590da292c5c97a3da386a5cf0dc266"),Nothing]

instance indexedEventRegisterCSC :: IndexedEvent (Tuple1 (Tagged (SProxy "callerAddress") Address)) (Tuple3 (Tagged (SProxy "csc") (BytesN (D1 :& D2))) (Tagged (SProxy "cscAddress") Address) (Tagged (SProxy "cscGeohash") (BytesN D8))) RegisterCSC where
  isAnonymous _ = false

derive instance genericRegisterCSC :: Generic RegisterCSC _

instance eventGenericRegisterCSCShow :: Show RegisterCSC where
	show = genericShow

instance eventGenericRegisterCSCeq :: Eq RegisterCSC where
	eq = genericEq