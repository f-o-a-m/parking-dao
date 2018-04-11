--------------------------------------------------------------------------------
-- | Nested.SimpleStorage
--------------------------------------------------------------------------------

module Contracts.Nested.SimpleStorage where

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
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, Tuple0(..), Tuple1(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | CountFn
--------------------------------------------------------------------------------


type CountFn = Tagged (SProxy "count()") (Tuple0 )

count :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
count x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CountFn)

--------------------------------------------------------------------------------
-- | SetCountFn
--------------------------------------------------------------------------------


type SetCountFn = Tagged (SProxy "setCount(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

setCount :: forall e. TransactionOptions NoPay -> { _count :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
setCount x0 r = uncurryFields  r $ setCount' x0
   where
    setCount' :: TransactionOptions NoPay -> Tagged (SProxy "_count") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    setCount' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetCountFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _count :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_count") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 (untagged y2 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | CountSet
--------------------------------------------------------------------------------


newtype CountSet = CountSet {_count :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeCountSet :: Newtype CountSet _

instance eventFilterCountSet :: EventFilter CountSet where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435")]

instance indexedEventCountSet :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "_count") (UIntN (D2 :& D5 :& D6)))) CountSet where
  isAnonymous _ = false

derive instance genericCountSet :: Generic CountSet _

instance eventGenericCountSetShow :: Show CountSet where
	show = genericShow

instance eventGenericCountSeteq :: Eq CountSet where
	eq = genericEq