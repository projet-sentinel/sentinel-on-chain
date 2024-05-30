module Oracle.ThreatDatabase.Types
    (   ThreatDatabaseParams(..)
    ,   ThreatDatabaseAddrListDatum(..)
    ,   ThreatDatabaseSHListDatum(..)
    ,   ThreatDatabaseAction(..)
    ,   ThreatDatabaseRefScriptDatum(..)
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

-- | Configuration parameters for the threat database.
--
-- Fields:
--
-- * 'builtinDatabaseIndex' - A unique identifier for the threat database.
-- * 'contractsControllerSH' - The script hash of the contracts controller.
-- * 'currencySymbolOfCAT' - The currency symbol for CAT tokens.
-- * 'currencySymbolOfTDAT' - The currency symbol for TDAT tokens.
-- * 'threatDatabaseDebuggerPKH' - The public key hash of the threat database debugger.
--
-- Note: This data type derives several common type classes for convenience.
data ThreatDatabaseParams = ThreatDatabaseParams
    { builtinDatabaseIndex      :: BuiltinByteString
    , contractsControllerSH     :: ScriptHash
    , currencySymbolOfCAT       :: CurrencySymbol
    , currencySymbolOfTDAT      :: CurrencySymbol
    , threatDatabaseDebuggerPKH :: PubKeyHash
    }
    deriving (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

PlutusTx.makeLift ''ThreatDatabaseParams

-- | Datum for the address list in the threat database.
--
-- Fields:
--
-- * 'threatTag' - A unique identifier for the threat.
-- * 'threatScore' - The score associated with the threat.
-- * 'threatData' - A list of addresses related to the threat.
-- * 'lastUpdated' - The last time the datum was updated.
--
-- Note: This data type implements a custom 'Eq' instance for comparison.
data ThreatDatabaseAddrListDatum
    =   ThreatDatabaseAddrListDatum
            {   threatTag   :: Integer
            ,   threatScore :: Integer
            ,   threatData  :: [Address] -- BuiltinData ([ScriptHash] / [AssetClass] / [Value])
            ,   lastUpdated :: POSIXTime
            }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

instance Eq ThreatDatabaseAddrListDatum where
    {-# INLINABLE (==) #-}
    (==) :: ThreatDatabaseAddrListDatum -> ThreatDatabaseAddrListDatum -> Bool
    ThreatDatabaseAddrListDatum a b c d == ThreatDatabaseAddrListDatum a' b' c' d'
        |   a == a' && b == b' && c == c' && d == d' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''ThreatDatabaseAddrListDatum    [ ( 'ThreatDatabaseAddrListDatum,  1 )
                                                            ]

-- | Datum for the script hash list in the threat database.
--
-- Fields:
--
-- * 'threatTag' - A unique identifier for the threat.
-- * 'threatScore' - The score associated with the threat.
-- * 'threatData' - A list of script hashes related to the threat.
-- * 'lastUpdated' - The last time the datum was updated.
--
-- Note: This data type implements a custom 'Eq' instance for comparison.
data ThreatDatabaseSHListDatum
    =   ThreatDatabaseSHListDatum
            {   threatTag   :: Integer
            ,   threatScore :: Integer
            ,   threatData  :: [ScriptHash]
            ,   lastUpdated :: POSIXTime
            }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

instance Eq ThreatDatabaseSHListDatum where
    {-# INLINABLE (==) #-}
    (==) :: ThreatDatabaseSHListDatum -> ThreatDatabaseSHListDatum -> Bool
    ThreatDatabaseSHListDatum a b c d == ThreatDatabaseSHListDatum a' b' c' d'
        |   a == a' && b == b' && c == c' && d == d' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''ThreatDatabaseSHListDatum    [ ( 'ThreatDatabaseSHListDatum,  1 )
                                                          ]


-- | Actions that can be performed on the threat database.
--
-- Constructors:
--
-- * 'UpdateThreatData' - Add a threat data threat data address to the threat database.
-- * 'RemoveThreatData' - Remove a threat data address from the threat database.
-- * 'ThreatDatabaseDebugger' - The threat database debugger action on the threat database.
--
-- Note: This data type defines the actions for manipulating the threat database.
data ThreatDatabaseAction
    =   UpdateThreatData
    |   RemoveThreatData
    |   ThreatDatabaseDebugger
    deriving (Haskell.Show, Generic, ToJSON)

PlutusTx.makeIsDataIndexed ''ThreatDatabaseAction   [ ( 'UpdateThreatData,          1 )
                                                    , ( 'RemoveThreatData,          2 )
                                                    , ( 'ThreatDatabaseDebugger,    3 )
                                                    ]

-- | Datum for the reference script in the threat database.
--
-- Fields:
--
-- * 'databaseIndex' - A unique identifier for the threat database.
-- * 'adminPKHs' - The public key hashes of the threat database admins.
-- * 'treasuryAddress' - The address which get paid the threat data UTxO.
--
-- Note: This data type derives several common type classes for convenience.
data ThreatDatabaseRefScriptDatum
    =   ThreatDatabaseRefScriptDatum
            {   databaseIndex   :: BuiltinByteString
            ,   adminPKHs       :: [PubKeyHash]
            ,   treasuryAddress :: Address
            }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

instance Eq ThreatDatabaseRefScriptDatum where
    {-# INLINABLE (==) #-}
    (==) :: ThreatDatabaseRefScriptDatum -> ThreatDatabaseRefScriptDatum -> Bool
    ThreatDatabaseRefScriptDatum a b c == ThreatDatabaseRefScriptDatum a' b' c'
        |   a == a' && b == b' && c == c' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''ThreatDatabaseRefScriptDatum [('ThreatDatabaseRefScriptDatum, 1)]
