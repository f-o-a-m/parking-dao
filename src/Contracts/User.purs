--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------

module Contracts.User where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D4, D5, D6, Tuple0(..), Tuple1(..), Tuple2, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | LastCheckInBlockFn
--------------------------------------------------------------------------------


type LastCheckInBlockFn = Tagged (SProxy "lastCheckInBlock()") (Tuple0 )

lastCheckInBlock :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
lastCheckInBlock x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: LastCheckInBlockFn)

--------------------------------------------------------------------------------
-- | RequestZoneFn
--------------------------------------------------------------------------------


type RequestZoneFn = Tagged (SProxy "requestZone(bytes4)") (Tuple1 (BytesN D4))

requestZone :: forall e. TransactionOptions NoPay -> { _zone :: (BytesN D4) } -> Web3 e HexString
requestZone x0 r = uncurryFields  r $ requestZone' x0
   where
    requestZone' :: TransactionOptions NoPay -> Tagged (SProxy "_zone") (BytesN D4) -> Web3 e HexString
    requestZone' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: RequestZoneFn)

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
-- | LicensedZonesFn
--------------------------------------------------------------------------------


type LicensedZonesFn = Tagged (SProxy "licensedZones(bytes4)") (Tuple1 (BytesN D4))

licensedZones :: forall e. TransactionOptions NoPay -> ChainCursor -> (BytesN D4) -> Web3 e (Either CallError Boolean)
licensedZones x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: LicensedZonesFn)

--------------------------------------------------------------------------------
-- | SetLastCheckInFn
--------------------------------------------------------------------------------


type SetLastCheckInFn = Tagged (SProxy "setLastCheckIn()") (Tuple0 )

setLastCheckIn :: forall e. TransactionOptions NoPay -> Web3 e HexString
setLastCheckIn x0 = sendTx x0 ((tagged $ Tuple0 ) :: SetLastCheckInFn)

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
-- | PendingAnchorFn
--------------------------------------------------------------------------------


type PendingAnchorFn = Tagged (SProxy "pendingAnchor()") (Tuple0 )

pendingAnchor :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
pendingAnchor x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: PendingAnchorFn)

--------------------------------------------------------------------------------
-- | PayForParkingFn
--------------------------------------------------------------------------------


type PayForParkingFn = Tagged (SProxy "payForParking(address)") (Tuple1 Address)

payForParking :: forall e. TransactionOptions Wei -> { _anchor :: Address } -> Web3 e HexString
payForParking x0 r = uncurryFields  r $ payForParking' x0
   where
    payForParking' :: TransactionOptions Wei -> Tagged (SProxy "_anchor") Address -> Web3 e HexString
    payForParking' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: PayForParkingFn)

--------------------------------------------------------------------------------
-- | LastCheckInFn
--------------------------------------------------------------------------------


type LastCheckInFn = Tagged (SProxy "lastCheckIn()") (Tuple0 )

lastCheckIn :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
lastCheckIn x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: LastCheckInFn)

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


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: forall e. TransactionOptions NoPay -> HexString -> Web3 e HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | CheckIn
--------------------------------------------------------------------------------


newtype CheckIn = CheckIn {user :: Address,anchor :: Address}

derive instance newtypeCheckIn :: Newtype CheckIn _

instance eventFilterCheckIn :: EventFilter CheckIn where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "f44d868c145e192ab2786eaa3827a434702c53e71182f67bb3915a85fe877f10")]

instance indexedEventCheckIn :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "user") Address) (Tagged (SProxy "anchor") Address)) CheckIn where
  isAnonymous _ = false

derive instance genericCheckIn :: Generic CheckIn _

instance eventGenericCheckInShow :: Show CheckIn where
	show = genericShow

instance eventGenericCheckIneq :: Eq CheckIn where
	eq = genericEq

--------------------------------------------------------------------------------
-- | ZoneGranted
--------------------------------------------------------------------------------


newtype ZoneGranted = ZoneGranted {zone :: (BytesN D4)}

derive instance newtypeZoneGranted :: Newtype ZoneGranted _

instance eventFilterZoneGranted :: EventFilter ZoneGranted where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "679a3fff4d6a82ec16b3e5047b4fd54c9afa27a8006139ec90644df4b7e51e78")]

instance indexedEventZoneGranted :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "zone") (BytesN D4))) ZoneGranted where
  isAnonymous _ = false

derive instance genericZoneGranted :: Generic ZoneGranted _

instance eventGenericZoneGrantedShow :: Show ZoneGranted where
	show = genericShow

instance eventGenericZoneGrantedeq :: Eq ZoneGranted where
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