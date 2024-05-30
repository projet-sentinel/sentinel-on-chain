
module  Oracle.TDAT.Types
            (   TDATParams         (..)
            ,   TDATAction         (..)
            ,   TDATRefScriptDatum (..)
            )
                where

import           Data.Aeson                 (ToJSON)
import           GHC.Generics               (Generic)
import           Ledger.Orphans             ()
import           PlutusLedgerApi.V1.Address (Address)
import           PlutusLedgerApi.V1.Crypto  (PubKeyHash)
import           PlutusLedgerApi.V1.Scripts (ScriptHash)
import           PlutusLedgerApi.V1.Value   (CurrencySymbol)
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString)
import           PlutusTx.Prelude           (Bool (..), Eq, (&&), (==))
import qualified Prelude                    as Haskell

-- | Configuration parameters for the TDAT contract.
--
-- Fields:
--
-- * 'builtinDatabaseIndex' - A unique identifier for the threat database.
-- * 'contractsControllerSH' - The script hash of the contracts controller.
-- * 'currencySymbolOfCAT' - The currency symbol for CAT tokens.
-- * 'tDATMinterDebuggerPKH' - The public key hash of the TDAT debugger.
--
-- Note: This data type derives several common type classes for convenience.
data TDATParams = TDATParams
    {   builtinDatabaseIndex  ::  BuiltinByteString
    ,   contractsControllerSH ::  ScriptHash
    ,   currencySymbolOfCAT   ::  CurrencySymbol
    ,   tDATMinterDebuggerPKH ::  PubKeyHash
    }
    deriving
        (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

PlutusTx.makeLift ''TDATParams

-- | Actions that can be performed on the TDAT contract.
--
-- Constructors:
--
-- * 'MintTDAT' - Mints a new TDAT token, which represents a threat database entry.
-- * 'BurnTDAT' - Burns an existing TDAT token, which means removing a threat database entry.
-- * 'TDATDebugger' - Update the score of an address in the threat database.
--
-- Note: This data type defines the actions for creation an removing TDAT token for the threat database.
data TDATAction
    =   MintTDAT
    |   BurnTDAT
    |   TDATDebugger
    deriving (Haskell.Show, Generic, ToJSON)

PlutusTx.makeIsDataIndexed ''TDATAction [ ( 'MintTDAT,     1 )
                                        , ( 'BurnTDAT,     2 )
                                        , ( 'TDATDebugger, 3 )
                                        ]


-- | Datum for the reference script in the threat database.
--
-- Fields:
--
-- * 'databaseIndex' - A unique identifier for the threat database.
-- * 'adminPKHs' - The public key hashes of the TDAT contract admins.
-- * 'threatDatabaseAddress' - The address which threat data UTxO will be sent to.
--
-- Note: This data type derives several common type classes for convenience.
data TDATRefScriptDatum
    =   TDATRefScriptDatum
            {   databaseIndex         ::  BuiltinByteString
            ,   adminPKHs             ::  [PubKeyHash]
            ,   threatDatabaseAddress ::  Address
            }
        deriving
        (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

instance Eq TDATRefScriptDatum where
    {-# INLINABLE (==) #-}
    (==) :: TDATRefScriptDatum -> TDATRefScriptDatum -> Bool
    TDATRefScriptDatum a b c == TDATRefScriptDatum a' b' c'
        |   a == a' &&  b == b' &&  c == c' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''TDATRefScriptDatum [('TDATRefScriptDatum, 1)]
