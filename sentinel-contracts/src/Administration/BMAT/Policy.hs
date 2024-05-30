module  Administration.BMAT.Policy
            (   mkPolicy
            ,   mkUntypedPolicy
            ,   compiledCode
            ,   BMATParams (..)
            )
                where

import           Data.Aeson                  (ToJSON)
import           GHC.Generics                (Generic)
import           Ledger.Orphans              ()
import           PlutusCore.Version          (plcVersion100)
import           PlutusLedgerApi.V1.Crypto   (PubKeyHash)
import           PlutusLedgerApi.V1.Tx       (TxId)
import           PlutusLedgerApi.V1.Value    (TokenName (..), flattenValue)
import           PlutusLedgerApi.V2.Contexts (ScriptContext (..), TxInInfo (..),
                                              TxInfo (..), TxOutRef (..),
                                              ownCurrencySymbol)
import           PlutusTx                    (BuiltinData, CompiledCode,
                                              applyCode, compile, liftCode,
                                              unsafeFromBuiltinData)
import qualified PlutusTx
import           PlutusTx.Builtins           (emptyByteString)
import           PlutusTx.Prelude            (Bool (..), Either (..), Integer,
                                              any, elem, error, filter, length,
                                              otherwise, (&&), (==))
import qualified Prelude                     as Haskell

data BMATParams = BMATParams
    {   boardMemberPKHs ::  [PubKeyHash]
    ,   inputUTxOId     ::  TxId
    ,   inputUTxOIdx    ::  Integer
    }
    deriving
        (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

PlutusTx.makeLift ''BMATParams

{-==============================================================================================================================-}
{-==========                                          MINTING POLICY SECTION                                          ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BMATParams -> () -> ScriptContext -> Bool
mkPolicy BMATParams{..} _ ctx@ScriptContext{ scriptContextTxInfo = TxInfo{..}} =
    let txOutRef' = TxOutRef inputUTxOId inputUTxOIdx
        emptyTokenName = TokenName emptyByteString
    in      any (`elem` txInfoSignatories) boardMemberPKHs
        &&  any (\i -> txInInfoOutRef i == txOutRef') txInfoInputs
        &&  length (filter (\(cs,tn,amt) -> cs == ownCurrencySymbol ctx && amt == 1 && tn == emptyTokenName) (flattenValue txInfoMint)) == 1
        &&  length (flattenValue txInfoMint) == 1

{-============================================== END OF MINTING POLICY SECTION =================================================-}

{-# INLINABLE mkUntypedPolicy #-}
mkUntypedPolicy :: BMATParams -> BuiltinData -> BuiltinData -> ()
mkUntypedPolicy params redeemer ctx
    |   mkPolicy params (PlutusTx.unsafeFromBuiltinData redeemer) (PlutusTx.unsafeFromBuiltinData ctx) = ()
    |   otherwise = error()

compiledCode :: BMATParams -> CompiledCode (BuiltinData -> BuiltinData -> ())
compiledCode params = case compiled of
    Right c -> c
    Left e  -> Haskell.error e
    where
        compiled = $$(PlutusTx.compile [||mkUntypedPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 params
