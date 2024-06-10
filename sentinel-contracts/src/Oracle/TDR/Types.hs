module Oracle.TDR.Types
    (   TDRParams(..)
    ,   TDRDatum(..)
    ,   TDRAction(..)
    ,   TDRRefScriptDatum(..)
    ) where

import           Data.Aeson                 (ToJSON)
import           GHC.Generics               (Generic)
import           Ledger.Orphans             ()
import           PlutusLedgerApi.V1.Address (Address)
import           PlutusLedgerApi.V1.Crypto  (PubKeyHash)
import           PlutusLedgerApi.V1.Scripts (ScriptHash)
import           PlutusLedgerApi.V1.Time    (POSIXTime)
import           PlutusLedgerApi.V1.Value   (CurrencySymbol)
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString)
import           PlutusTx.Prelude           (Bool (..), Eq, Integer, (&&), (==))
import qualified Prelude                    as Haskell

-- | Configuration parameters for the threat detection and response (TDR).
--
-- Fields:
--
-- * 'builtinTDRIndex' - A unique identifier for the threat detection and response (TDR).
-- * 'contractsControllerSH' - The script hash of the contracts controller.
-- * 'currencySymbolOfCAT' - The currency symbol for CAT tokens.
-- * 'currencySymbolOfTDAT' - The currency symbol for TDAT tokens.
-- * 'TDRDebuggerPKH' - The public key hash of the threat detection and response (TDR) debugger.
--
-- Note: This data type derives several common type classes for convenience.
data TDRParams = TDRParams
    { builtinTDRIndex       :: BuiltinByteString
    , contractsControllerSH :: ScriptHash
    , currencySymbolOfCAT   :: CurrencySymbol
    , currencySymbolOfTDAT  :: CurrencySymbol
    , tDRDebuggerPKH        :: PubKeyHash
    }
    deriving (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

PlutusTx.makeLift ''TDRParams

-- | Datum Of the threat detection and response (TDR).
--
-- Fields:
--
-- * 'requesterAddress' - Address of the requester which in case of denying (RemoveRequest) the UTxO will be sent to the address of the requester.
-- * 'threatScore' - The score associated with the threat.
-- * 'threatData' - An address related to the threat.
-- * 'requestTime' - The time the request was made.
--
-- Note: This data type implements a custom 'Eq' instance for comparison.
data TDRDatum
    =   TDRDatum
            {   requesterAddress :: Address
            ,   threatScore      :: Integer
            ,   threatData       :: Address
            ,   requestTime      :: POSIXTime
            }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

instance Eq TDRDatum where
    {-# INLINABLE (==) #-}
    (==) :: TDRDatum -> TDRDatum -> Bool
    TDRDatum a b c d == TDRDatum a' b' c' d'
        |   a == a' && b == b' && c == c' && d == d' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''TDRDatum   [( 'TDRDatum,  1 )]


-- | Actions that can be performed on the threat detection and response (TDR).
--
-- Constructors:
--
-- * 'AcceptRequest' - Accept request and add a threat data to the threat database.
-- * 'RemoveRequest' - Remove request from the threat detection and response (TDR).
-- * 'TDRDebugger' - The threat detection and response (TDR) debugger action on the threat database.
--
-- Note: This data type defines the actions for manipulating the threat detection and response (TDR).
data TDRAction
    =   AcceptRequest
    |   RemoveRequest
    |   TDRDebugger
    deriving (Haskell.Show, Generic, ToJSON)

PlutusTx.makeIsDataIndexed ''TDRAction  [ ( 'AcceptRequest,     1 )
                                        , ( 'RemoveRequest,     2 )
                                        , ( 'TDRDebugger,       3 )
                                        ]

-- | Datum for the reference script in the threat detection and response (TDR).
--
-- Fields:
--
-- * 'tDRIndex' - A unique identifier for the threat detection and response (TDR).
-- * 'databaseIndex' - A unique identifier for the threat database.
-- * 'threatDatabaseSH' - The script hash of threat database contract which threat data will be added to.
-- * 'requestPeriod' - The time period in POSIX time (milliseconds) for the request.
--
-- Note: This data type derives several common type classes for convenience.
data TDRRefScriptDatum
    =   TDRRefScriptDatum
            {   tDRIndex         :: BuiltinByteString
            ,   databaseIndex    :: BuiltinByteString
            ,   threatDatabaseSH :: ScriptHash
            ,   requestPeriod    :: POSIXTime
            }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

instance Eq TDRRefScriptDatum where
    {-# INLINABLE (==) #-}
    (==) :: TDRRefScriptDatum -> TDRRefScriptDatum -> Bool
    TDRRefScriptDatum a b c e == TDRRefScriptDatum a' b' c' e'
        |   a == a' && b == b' && c == c' && e == e' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''TDRRefScriptDatum [('TDRRefScriptDatum, 1)]
