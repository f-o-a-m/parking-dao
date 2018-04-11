--------------------------------------------------------------------------------
-- | ParkingAnchor
--------------------------------------------------------------------------------

module Contracts.ParkingAnchor where

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
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D8, Tuple0(..), Tuple1(..), Tuple2(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | GeohashFn
--------------------------------------------------------------------------------


type GeohashFn = Tagged (SProxy "geohash()") (Tuple0 )

geohash :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN D8))
geohash x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GeohashFn)

--------------------------------------------------------------------------------
-- | TransferBalanceToOwnerFn
--------------------------------------------------------------------------------


type TransferBalanceToOwnerFn = Tagged (SProxy "transferBalanceToOwner()") (Tuple0 )

transferBalanceToOwner :: forall e. TransactionOptions NoPay -> Web3 e HexString
transferBalanceToOwner x0 = sendTx x0 ((tagged $ Tuple0 ) :: TransferBalanceToOwnerFn)

--------------------------------------------------------------------------------
-- | AnchorIdFn
--------------------------------------------------------------------------------


type AnchorIdFn = Tagged (SProxy "anchorId()") (Tuple0 )

anchorId :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& D2)))
anchorId x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AnchorIdFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | ParkingAuthorityFn
--------------------------------------------------------------------------------


type ParkingAuthorityFn = Tagged (SProxy "parkingAuthority()") (Tuple0 )

parkingAuthority :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
parkingAuthority x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ParkingAuthorityFn)

--------------------------------------------------------------------------------
-- | AcceptPaymentFn
--------------------------------------------------------------------------------


type AcceptPaymentFn = Tagged (SProxy "acceptPayment()") (Tuple0 )

acceptPayment :: forall e. TransactionOptions Wei -> Web3 e HexString
acceptPayment x0 = sendTx x0 ((tagged $ Tuple0 ) :: AcceptPaymentFn)

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
-- | CscFn
--------------------------------------------------------------------------------


type CscFn = Tagged (SProxy "csc()") (Tuple0 )

csc :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D1 :& D2)))
csc x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CscFn)

--------------------------------------------------------------------------------
-- | TransferOwnershipFn
--------------------------------------------------------------------------------


type TransferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 Address)

transferOwnership :: forall e. TransactionOptions NoPay -> { newOwner :: Address } -> Web3 e HexString
transferOwnership x0 r = uncurryFields  r $ transferOwnership' x0
   where
    transferOwnership' :: TransactionOptions NoPay -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: TransferOwnershipFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(bytes8,bytes32)") (Tuple2 (BytesN D8) (BytesN (D3 :& D2)))

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _geohash :: (BytesN D8), _anchorId :: (BytesN (D3 :& D2)) } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_geohash") (BytesN D8) -> Tagged (SProxy "_anchorId") (BytesN (D3 :& D2)) -> Web3 e HexString
    constructor' y0 bc' y2 y3 = deployContract y0 bc' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | PaymentAccepted
--------------------------------------------------------------------------------


newtype PaymentAccepted = PaymentAccepted {anchor :: Address,user :: Address}

derive instance newtypePaymentAccepted :: Newtype PaymentAccepted _

instance eventFilterPaymentAccepted :: EventFilter PaymentAccepted where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e508aabf86caa3d4a6d688daaba1557710255b898343f7f8b71765d80da25a80")]

instance indexedEventPaymentAccepted :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "anchor") Address) (Tagged (SProxy "user") Address)) PaymentAccepted where
  isAnonymous _ = false

derive instance genericPaymentAccepted :: Generic PaymentAccepted _

instance eventGenericPaymentAcceptedShow :: Show PaymentAccepted where
	show = genericShow

instance eventGenericPaymentAcceptedeq :: Eq PaymentAccepted where
	eq = genericEq

--------------------------------------------------------------------------------
-- | OwnershipTransferred
--------------------------------------------------------------------------------


newtype OwnershipTransferred = OwnershipTransferred {previousOwner :: Address,newOwner :: Address}

derive instance newtypeOwnershipTransferred :: Newtype OwnershipTransferred _

instance eventFilterOwnershipTransferred :: EventFilter OwnershipTransferred where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8be0079c531659141344cd1fd0a4f28419497f9722a3daafe3b4186f6b6457e0"),Nothing,Nothing]

instance indexedEventOwnershipTransferred :: IndexedEvent (Tuple2 (Tagged (SProxy "previousOwner") Address) (Tagged (SProxy "newOwner") Address)) (Tuple0 ) OwnershipTransferred where
  isAnonymous _ = false

derive instance genericOwnershipTransferred :: Generic OwnershipTransferred _

instance eventGenericOwnershipTransferredShow :: Show OwnershipTransferred where
	show = genericShow

instance eventGenericOwnershipTransferredeq :: Eq OwnershipTransferred where
	eq = genericEq