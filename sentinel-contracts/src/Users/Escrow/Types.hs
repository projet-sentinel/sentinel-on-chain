module  Users.Escrow.Types
            (   EscrowParams            (..)
            ,   EscrowDatum             (..)
            )
                where

import           Data.Aeson                 (ToJSON)
import           GHC.Generics               (Generic)
import           Ledger.Orphans             ()
import           PlutusLedgerApi.V1.Address (Address)
import           PlutusLedgerApi.V1.Crypto  (PubKeyHash)
import           PlutusLedgerApi.V1.Value   (CurrencySymbol)
import qualified PlutusTx
import           PlutusTx.Prelude           (Bool (..), Eq, (&&), (==))
import qualified Prelude                    as Haskell

-- | Configuration parameters for the Escrow contract.
--
-- Fields:
--
-- * 'currencySymbolOfTDAT' - The currency symbol for TDAT tokens.
-- * 'threatDatabaseAddress' - The address which threat data UTxO will be read from.
--
-- Note: This data type derives several common type classes for convenience.
data EscrowParams = EscrowParams
    {   currencySymbolOfTDAT  ::  CurrencySymbol
    ,   threatDatabaseAddress ::  Address
    }
    deriving
        (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

PlutusTx.makeLift ''EscrowParams

-- | Datum for the Escrow contract.
--
-- Fields:
--
-- * 'senderAddress' - The address of the sender UTxO to be escrowed.
-- * 'receiverAddress' - The address of the receiver of escrowed UTxO.
--
-- Note: This data type implements a custom 'Eq' instance for comparison.
data EscrowDatum
    =   EscrowDatum
            {   senderAddress   :: Address
            ,   receiverAddress :: Address
            }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

instance Eq EscrowDatum where
    {-# INLINABLE (==) #-}
    (==) :: EscrowDatum -> EscrowDatum -> Bool
    EscrowDatum a b == EscrowDatum a' b'
        |   a == a' && b == b' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''EscrowDatum    [ ( 'EscrowDatum,  1 )]
