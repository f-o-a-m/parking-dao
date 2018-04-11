--------------------------------------------------------------------------------
-- | ParkingAuthority
--------------------------------------------------------------------------------

module Contracts.ParkingAuthority where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D4, D8, Tuple0(..), Tuple1(..), Tuple2(..), Tuple4, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | RegisterParkingAnchorFn
--------------------------------------------------------------------------------


type RegisterParkingAnchorFn = Tagged (SProxy "registerParkingAnchor(bytes8,bytes32)") (Tuple2 (BytesN D8) (BytesN (D3 :& D2)))

registerParkingAnchor :: forall e. TransactionOptions NoPay -> { _geohash :: (BytesN D8), _anchorId :: (BytesN (D3 :& D2)) } -> Web3 e HexString
registerParkingAnchor x0 r = uncurryFields  r $ registerParkingAnchor' x0
   where
    registerParkingAnchor' :: TransactionOptions NoPay -> Tagged (SProxy "_geohash") (BytesN D8) -> Tagged (SProxy "_anchorId") (BytesN (D3 :& D2)) -> Web3 e HexString
    registerParkingAnchor' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: RegisterParkingAnchorFn)

--------------------------------------------------------------------------------
-- | RegisterUserFn
--------------------------------------------------------------------------------


type RegisterUserFn = Tagged (SProxy "registerUser()") (Tuple0 )

registerUser :: forall e. TransactionOptions NoPay -> Web3 e HexString
registerUser x0 = sendTx x0 ((tagged $ Tuple0 ) :: RegisterUserFn)

--------------------------------------------------------------------------------
-- | AnchorsFn
--------------------------------------------------------------------------------


type AnchorsFn = Tagged (SProxy "anchors(address)") (Tuple1 Address)

anchors :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
anchors x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AnchorsFn)

--------------------------------------------------------------------------------
-- | AddZoneFn
--------------------------------------------------------------------------------


type AddZoneFn = Tagged (SProxy "addZone(bytes4)") (Tuple1 (BytesN D4))

addZone :: forall e. TransactionOptions NoPay -> { _zone :: (BytesN D4) } -> Web3 e HexString
addZone x0 r = uncurryFields  r $ addZone' x0
   where
    addZone' :: TransactionOptions NoPay -> Tagged (SProxy "_zone") (BytesN D4) -> Web3 e HexString
    addZone' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: AddZoneFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | UsersFn
--------------------------------------------------------------------------------


type UsersFn = Tagged (SProxy "users(address)") (Tuple1 Address)

users :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
users x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: UsersFn)

--------------------------------------------------------------------------------
-- | ParkingCSRFn
--------------------------------------------------------------------------------


type ParkingCSRFn = Tagged (SProxy "parkingCSR()") (Tuple0 )

parkingCSR :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
parkingCSR x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ParkingCSRFn)

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


type ConstructorFn = Tagged (SProxy "constructor(address)") (Tuple1 Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { foamCSR :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "foamCSR") Address -> Web3 e HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 (untagged y2 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | RegisteredParkingAnchor
--------------------------------------------------------------------------------


newtype RegisteredParkingAnchor = RegisteredParkingAnchor {owner :: Address,anchor :: Address,geohash :: (BytesN D8),anchorId :: (BytesN (D3 :& D2))}

derive instance newtypeRegisteredParkingAnchor :: Newtype RegisteredParkingAnchor _

instance eventFilterRegisteredParkingAnchor :: EventFilter RegisteredParkingAnchor where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "aab6918dd8fd3f309a5a35d2b548ed336de03cb91bbb1cb0483830eba1d1d07c")]

instance indexedEventRegisteredParkingAnchor :: IndexedEvent (Tuple0 ) (Tuple4 (Tagged (SProxy "owner") Address) (Tagged (SProxy "anchor") Address) (Tagged (SProxy "geohash") (BytesN D8)) (Tagged (SProxy "anchorId") (BytesN (D3 :& D2)))) RegisteredParkingAnchor where
  isAnonymous _ = false

derive instance genericRegisteredParkingAnchor :: Generic RegisteredParkingAnchor _

instance eventGenericRegisteredParkingAnchorShow :: Show RegisteredParkingAnchor where
	show = genericShow

instance eventGenericRegisteredParkingAnchoreq :: Eq RegisteredParkingAnchor where
	eq = genericEq

--------------------------------------------------------------------------------
-- | RegisterParkingUser
--------------------------------------------------------------------------------


newtype RegisterParkingUser = RegisterParkingUser {owner :: Address,user :: Address}

derive instance newtypeRegisterParkingUser :: Newtype RegisterParkingUser _

instance eventFilterRegisterParkingUser :: EventFilter RegisterParkingUser where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "f60a6e454f5351ba1db339f0bdbcb8fb8bfd72c5ef6e277c56f72682f84f2171")]

instance indexedEventRegisterParkingUser :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "owner") Address) (Tagged (SProxy "user") Address)) RegisterParkingUser where
  isAnonymous _ = false

derive instance genericRegisterParkingUser :: Generic RegisterParkingUser _

instance eventGenericRegisterParkingUserShow :: Show RegisterParkingUser where
	show = genericShow

instance eventGenericRegisterParkingUsereq :: Eq RegisterParkingUser where
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