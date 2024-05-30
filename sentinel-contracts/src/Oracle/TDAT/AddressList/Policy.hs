
module  Oracle.TDAT.AddressList.Policy
            (   mkPolicy
            ,   mkUntypedPolicy
            ,   compiledCode
            )
                where

import           PlutusCore.Version                       (plcVersion100)
import           PlutusLedgerApi.V1.Address               (Address (..))
import           PlutusLedgerApi.V1.Credential            (Credential (..))
import           PlutusLedgerApi.V1.Interval              (contains, to)
import           PlutusLedgerApi.V1.Scripts               (Datum (..),
                                                           ScriptHash (..))
import           PlutusLedgerApi.V1.Value                 (CurrencySymbol (..),
                                                           TokenName (..),
                                                           flattenValue,
                                                           valueOf)
import           PlutusLedgerApi.V2.Contexts              (ScriptContext (..),
                                                           TxInInfo (..),
                                                           TxInfo (..),
                                                           TxOut (..),
                                                           ownCurrencySymbol)
import           PlutusLedgerApi.V2.Tx                    (OutputDatum (..))
import           PlutusTx                                 (BuiltinData,
                                                           CompiledCode,
                                                           UnsafeFromData,
                                                           applyCode, compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.Prelude                         (Bool (..),
                                                           Either (..), Integer,
                                                           Maybe (..), any,
                                                           elem, error, filter,
                                                           find, length,
                                                           otherwise,
                                                           traceError,
                                                           traceIfFalse, ($),
                                                           (&&), (==))
import qualified Prelude                                  as Haskell

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           Oracle.TDAT.Types                        (TDATAction (..),
                                                           TDATParams (..),
                                                           TDATRefScriptDatum (..))
import           Oracle.ThreatDatabase.Types              (ThreatDatabaseAddrListDatum (..))

{-==============================================================================================================================-}
{-==========                                          MINTING POLICY SECTION                                          ==========-}
{-==============================================================================================================================-}


{-# INLINABLE mkPolicy #-}
{-|
    The `mkPolicy` function is the main validator for the TDAT contract.
    It takes the contract parameters and the script context as input,
    and performs various checks and actions based on the redeemer value.

    Parameters:
        - 'TDATParams': A record containing the necessary parameters for the TDAT contract.
        - 'datum': The datum associated with the current transaction output.
        - 'redeemer': The redeemer value associated with the current transaction.
        - 'ctx': The script context containing information about the current transaction.

    The function performs the following actions based on the 'redeemer' and 'datum' values:

    1. `MintTDAT`: Mints a new TDAT token, which represents a threat database entry. This action is only allowed if the following conditions are met:
        - Any of the transaction signatories are in the list of admin public key hashes.
        - The TDAT token was minted or burnt exactly 1 time.
        - The last updated timestamp of the threat database entry is within the transaction's validity range.

    2. `BurnTDAT`: Burns an existing TDAT token. This action is only allowed if the following conditions are met:
        - The TDAT token was minted or burnt exactly 1 time (in the negative direction).
        - Any of the transaction signatories are in the list of admin public key hashes.

    3. `TDATDebugger`: Allows the TDAT minter debugger to perform some debugging actions.
        - This action is only allowed if the TDAT minter debugger public key hash is in the list of transaction signatories.

    Example usage:
        mkPolicy
            TDATParams
                { builtinDatabaseIndex = 123
                , contractsControllerSH = "05514ec851b6f7ca902dd88dd5a78444892e60d06746f63dc79e4877"
                , currencySymbolOfCAT = "05514ec851b6f7ca902dd88dd5a78444892e60d06746f63dc79e4877"
                , tDATMinterDebuggerPKH = "05514ec851b6f7ca902dd88dd5a78444892e60d06746f63dc79e4877"
                }
            MintTDAT
            (ScriptContext { scriptContextTxInfo = TxInfo { ... } })

    Note:
        - The function uses various helper functions to retrieve data from transaction outputs, calculate values, and perform checks.
        - Error handling is done using 'traceError' and 'traceIfFalse' functions.
        - This code is part of the Plutus-based Threat Database Address List contract implementations.
-}
mkPolicy :: TDATParams -> TDATAction -> ScriptContext -> Bool
mkPolicy TDATParams{..} redeemer ctx@ScriptContext{ scriptContextTxInfo = TxInfo{..}}
    |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
    ,   TDATRefScriptDatum databaseIndex adminPKHs threatDatabaseAddress <- unsafeFromBuiltinData refData
    ,   traceIfFalse "TDAT 1" (builtinDatabaseIndex == databaseIndex)
        =
            case redeemer of

                -----------------------------------------------------------------------------------------
                --                                 ISSUE THREAT DATA                                   --
                -----------------------------------------------------------------------------------------

                MintTDAT
                    |   ThreatDatabaseAddrListDatum _ _ _ lastUpdated <- getTxOutInlineDatum $ getThreatDataTxOut threatDatabaseAddress
                        ->
                                traceIfFalse "TDAT 1.1"
                                    (any (`elem` txInfoSignatories) adminPKHs)
                            &&  traceIfFalse "TDAT 1.2"
                                    (wasMintedBurntExactly 1)
                            &&  traceIfFalse "TDAT 1.3"
                                    (to lastUpdated `contains` txInfoValidRange)

                -----------------------------------------------------------------------------------------
                --                                  REMOVE THREAT DATA                                 --
                -----------------------------------------------------------------------------------------

                BurnTDAT ->
                        traceIfFalse "TDAT 2.2"
                            (wasMintedBurntExactly (-1))
                    &&  traceIfFalse "TDAT 2.3"
                            (any (`elem` txInfoSignatories) adminPKHs)

                -----------------------------------------------------------------------------------------
                --                                 DEBUGGER ACTION                                     --
                -----------------------------------------------------------------------------------------

                TDATDebugger ->
                    traceIfFalse "TDAT 3.1"
                        (tDATMinterDebuggerPKH `elem` txInfoSignatories)

    |   otherwise = traceError "TDAT 2"

    where

        getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
        getTxOutInlineDatum tx
            | ( OutputDatum ( Datum inline )) <- txOutDatum tx
            = unsafeFromBuiltinData @dat inline
            | otherwise = traceError "TDAT 3"


        wasMintedBurntExactly :: Integer -> Bool
        wasMintedBurntExactly amt =
                length (filter (\(cs,tn,amt') -> cs == ownCurrencySymbol ctx && tn == TokenName builtinDatabaseIndex && amt == amt') (flattenValue txInfoMint)) == 1
            &&  length (flattenValue txInfoMint) == 1


        getScriptRefTxOut :: TxOut
        getScriptRefTxOut
            |   Just i <- find  (   \i ->
                                        (   case txOutAddress (txInInfoResolved i) of
                                                (Address (ScriptCredential sh) _) -> sh == contractsControllerSH
                                                _ -> False
                                        )
                                    &&
                                        fromJust (txOutReferenceScript (txInInfoResolved i)) == currencySymbolToScriptHash (ownCurrencySymbol ctx)
                                    &&
                                        valueOf (txOutValue (txInInfoResolved i)) currencySymbolOfCAT (currencySymbolToTokenName (ownCurrencySymbol ctx)) == 1
                                )
                                txInfoReferenceInputs = txInInfoResolved i
            |   otherwise = traceError "TDAT 4"
                where
                    fromJust :: Maybe a -> a
                    fromJust (Just x) = x
                    fromJust _        = traceError "TDAT 5"

                    currencySymbolToScriptHash :: CurrencySymbol -> ScriptHash
                    currencySymbolToScriptHash (CurrencySymbol hash) = ScriptHash hash

                    currencySymbolToTokenName :: CurrencySymbol -> TokenName
                    currencySymbolToTokenName (CurrencySymbol hash) = TokenName hash


        getThreatDataTxOut :: Address -> TxOut
        getThreatDataTxOut addr
            |   Just o <- find  (  \o ->
                                            valueOf (txOutValue o) (ownCurrencySymbol ctx) (TokenName builtinDatabaseIndex) == 1
                                        &&
                                            txOutAddress o == addr
                                )
                                txInfoOutputs = o
            |   otherwise = traceError "TDAT 6"


{-============================================== END OF MINTING POLICY SECTION =================================================-}

-------------------------------------------------------------------------------------------
--                                     COMPILATION                                       --
-------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedPolicy #-}
mkUntypedPolicy :: TDATParams -> BuiltinData -> BuiltinData -> ()
mkUntypedPolicy params redeemer ctx
    |   mkPolicy params (PlutusTx.unsafeFromBuiltinData redeemer) (PlutusTx.unsafeFromBuiltinData ctx) = ()
    |   otherwise = error()

compiledCode :: TDATParams -> CompiledCode (BuiltinData -> BuiltinData -> ())
compiledCode params = case compiled of
    Right c -> c
    Left e  -> Haskell.error e
    where
        compiled = $$(PlutusTx.compile [||mkUntypedPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 params
