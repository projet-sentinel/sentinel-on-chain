module Oracle.ThreatDatabase.ScriptHashList.Validator
    (   mkValidator
    ,   mkUntypedValidator
    ,   compiledCode
    )   where

import           Data.Maybe                               (fromMaybe)
import           PlutusCore.Version                       (plcVersion100)
import           PlutusLedgerApi.V1.Address               (Address (..))
import           PlutusLedgerApi.V1.Credential            (Credential (..))
import           PlutusLedgerApi.V1.Scripts               (Datum (..),
                                                           ScriptHash (..),
                                                           getRedeemer)
import           PlutusLedgerApi.V1.Value                 (CurrencySymbol (..),
                                                           TokenName (..),
                                                           Value (..),
                                                           adaSymbol, adaToken)
import           PlutusLedgerApi.V2.Contexts              (ScriptContext (..),
                                                           ScriptPurpose (..),
                                                           TxInInfo (..),
                                                           TxInfo (..),
                                                           TxOut (..),
                                                           findOwnInput,
                                                           getContinuingOutputs)
import           PlutusLedgerApi.V2.Tx                    (OutputDatum (..))
import           PlutusTx                                 (BuiltinData,
                                                           CompiledCode,
                                                           UnsafeFromData,
                                                           applyCode, compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.AssocMap                        (lookup)
import           PlutusTx.Prelude                         (Bool (..),
                                                           Either (..), Integer,
                                                           Maybe (..), any,
                                                           elem, error, filter,
                                                           find, map, otherwise,
                                                           traceError,
                                                           traceIfFalse, ($),
                                                           (&&), (<), (==))
import qualified Prelude                                  as Haskell

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           Oracle.TDAT.Types                        (TDATAction (..))
import           Oracle.ThreatDatabase.Types              (ThreatDatabaseAction (..),
                                                           ThreatDatabaseParams (..),
                                                           ThreatDatabaseRefScriptDatum (..),
                                                           ThreatDatabaseSHListDatum (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
{-|
    The 'mkValidator' function is the main validator for the ThreatDatabase script hashes list contract.
    It handles various actions related to updating, removing, and debugging threat data.

    Parameters:
        - 'ThreatDatabaseParams': A record containing the necessary parameters for the ThreatDatabase contract.
        - 'datum': The datum associated with the current transaction output.
        - 'redeemer': The redeemer value associated with the current transaction.
        - 'ctx': The script context containing information about the current transaction.

    The function performs the following actions based on the 'redeemer' and 'datum' values:

    1. UpdateThreatData:
        - Checks if the transaction is signed by an admin.
        - Verifies that the output contains exactly one token of the correct currency symbol and token name.
        - Ensures that the input and output UTxOs are correct.
        - Checks if the 'lastUpdated' value in the output datum is greater than the input datum.
        - Verifies that the 'tag' values in the input and output data are the same.

    2. RemoveThreatData:
        - Checks if the transaction is signed by an admin.
        - Ensures that the treasury address is getting paid the exact ADA amount from the input UTxO.

    3. ThreatDatabaseDebugger:
        - Checks if the transaction is signed by the threat database debugger public key hash.

    4. Any other case:
        - Fails the validation.

    Example usage:
        mkValidator
            ThreatDatabaseParams
                {   builtinDatabaseIndex = 123
                ,   contractsControllerSH = "05514ec851b6f7ca902dd88dd5a78444892e60d06746f63dc79e4877"
                ,   currencySymbolOfCAT = "05514ec851b6f7ca902dd88dd5a78444892e60d06746f63dc79e4877"
                ,   currencySymbolOfTDAT = "05514ec851b6f7ca902dd88dd5a78444892e60d06746f63dc79e4877"
                ,   threatDatabaseDebuggerPKH = "05514ec851b6f7ca902dd88dd5a78444892e60d06746f63dc79e4877"
                }
            (ThreatDatabaseSHListDatum 123 123 [05514ec851b6f7ca902dd88dd5a78444892e60d06746f63dc79e4877] 1234567890)
            UpdateThreatData
            (ScriptContext { scriptContextTxInfo = TxInfo { ... } })

  Note:
    - The function uses various helper functions to retrieve data from transaction outputs, calculate values, and perform checks.
    - Error handling is done using 'traceError' and 'traceIfFalse' functions.
    - The code assumes that certain conditions are met, such as the existence of a single script input and output UTxO.
-}
mkValidator :: ThreatDatabaseParams -> ThreatDatabaseSHListDatum -> ThreatDatabaseAction -> ScriptContext -> Bool
mkValidator ThreatDatabaseParams{..} datum redeemer ctx@ScriptContext{ scriptContextTxInfo = TxInfo{..} }
    |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
    ,   ThreatDatabaseRefScriptDatum databaseIndex adminPKHs treasuryAddress <- unsafeFromBuiltinData refData
    ,   traceIfFalse "ThreatDatabase 1" (builtinDatabaseIndex == databaseIndex)
        =
            case (redeemer, datum) of

                -----------------------------------------------------------------------------------------
                --                                 UPDATE THREAT DATA                                  --
                -----------------------------------------------------------------------------------------

                (UpdateThreatData,
                    ThreatDatabaseSHListDatum tag _ _ lastUpdated )
                        |   ThreatDatabaseSHListDatum tag' _ _ lastUpdated' <- getTxOutInlineDatum singleScriptOutputUTxO
                            ->
                                    traceIfFalse "ThreatDatabase 1.1"
                                        (any (`elem` txInfoSignatories) adminPKHs)
                                &&  traceIfFalse "ThreatDatabase 1.2"
                                        (valueOf (txOutValue singleScriptOutputUTxO) currencySymbolOfTDAT (TokenName builtinDatabaseIndex) == 1)
                                &&  traceIfFalse "ThreatDatabase 1.3"
                                        isSCInputOutputCorrect
                                &&  traceIfFalse "ThreatDatabase 1.4"
                                        (lastUpdated < lastUpdated')
                                &&  traceIfFalse "ThreatDatabase 1.5"
                                        (tag == tag')

                -----------------------------------------------------------------------------------------
                --                                 REMOVE THREAT DATA                                  --
                -----------------------------------------------------------------------------------------

                (RemoveThreatData,
                    ThreatDatabaseSHListDatum{})
                        |   BurnTDAT <- getTDATRedeemer ->
                                    traceIfFalse "ThreatDatabase 2.1"
                                        (any (`elem` txInfoSignatories) adminPKHs)
                                &&  traceIfFalse "ThreatDatabase 2.2"
                                        (isAddrGettingPaidExactly treasuryAddress (adaAmountOf (txOutValue exactlyOneScriptInputUTxO)))


                -----------------------------------------------------------------------------------------
                --                             THREAT DATABASE DEBUGGER ACTION                         --
                -----------------------------------------------------------------------------------------

                (ThreatDatabaseDebugger, _) ->
                    traceIfFalse "ThreatDatabase 3.1"
                        $ threatDatabaseDebuggerPKH  `elem` txInfoSignatories

                -----------------------------------------------------------------------------------------
                --                                   ANY OTHER CASE                                    --
                -----------------------------------------------------------------------------------------

                (_,_) -> traceIfFalse "ThreatDatabase 4.1" False

    |   otherwise = traceError "ThreatDatabase 2"

            where

                getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
                getTxOutInlineDatum tx
                    | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                    = unsafeFromBuiltinData @dat inline
                    | otherwise = traceError "ThreatDatabase 3"

                valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
                valueOf (Value mp) cur tn
                    |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                    |   otherwise = 0

                singleScriptInputUTxO :: TxOut
                singleScriptInputUTxO
                    |   Just i      <-  findOwnInput ctx = txInInfoResolved i
                    |   otherwise   =   traceError "ThreatDatabase 4"

                singleScriptOutputUTxO :: TxOut
                singleScriptOutputUTxO
                    |   [o]         <-  getContinuingOutputs ctx = o
                    |   otherwise   =   traceError "ThreatDatabase 5"

                adaAmountOf :: Value -> Integer
                adaAmountOf val = valueOf val adaSymbol adaToken

                ownHashes :: TxOut -> ScriptHash
                ownHashes TxOut{txOutAddress=Address (ScriptCredential s) _} = s
                ownHashes _ = traceError "ThreatDatabase 6"

                ownHash :: ScriptHash
                ownHash = ownHashes singleScriptInputUTxO
                getScriptRefTxOut :: TxOut
                getScriptRefTxOut
                        |   Just i <- find  (   \i ->
                                                    (   case txOutAddress (txInInfoResolved i) of
                                                            (Address (ScriptCredential sh) _) -> sh == contractsControllerSH
                                                            _ -> False
                                                    )
                                                &&
                                                    fromJust (txOutReferenceScript (txInInfoResolved i)) == ownHash
                                                &&
                                                    valueOf (txOutValue (txInInfoResolved i)) currencySymbolOfCAT (scriptHashToTokenName ownHash) == 1
                                            )
                                            txInfoReferenceInputs = txInInfoResolved i
                        |   otherwise = traceError "ThreatDatabase 7"
                            where

                                scriptHashToTokenName :: ScriptHash -> TokenName
                                scriptHashToTokenName (ScriptHash hash) = TokenName hash

                                fromJust :: Maybe a -> a
                                fromJust (Just x) = x
                                fromJust _        = traceError "ThreatDatabase 8"

                isSCInputOutputCorrect :: Bool
                isSCInputOutputCorrect =
                        txOutValue singleScriptOutputUTxO == txOutValue singleScriptInputUTxO

                isAddrGettingPaidExactly :: Address -> Integer -> Bool
                isAddrGettingPaidExactly addr int =
                    any (\o -> txOutAddress o == addr && adaAmountOf (txOutValue o) == int) txInfoOutputs


                getTDATRedeemer :: forall rad. UnsafeFromData rad => rad
                getTDATRedeemer
                    |   Just rawMintingRedeemer <- lookup (Minting currencySymbolOfTDAT) txInfoRedeemers
                    =   unsafeFromBuiltinData @rad $ getRedeemer rawMintingRedeemer
                    |   otherwise = traceError "ThreatDatabase 9"

                exactlyOneScriptInputUTxO :: TxOut
                exactlyOneScriptInputUTxO
                    | [tx] <- filter (\tx -> txOutAddress tx == txOutAddress singleScriptInputUTxO) (map txInInfoResolved txInfoInputs) = tx
                    | otherwise = traceError "ThreatDatabase 10"


{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
-- |                                  COMPILATION                                     | --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: ThreatDatabaseParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedValidator params datum redeemer ctx
    |   mkValidator params (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx) = ()
    |   otherwise = error()

compiledCode :: ThreatDatabaseParams -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledCode params = case compiled of
    Right c -> c
    Left e  -> Haskell.error e
    where
        compiled = $$(PlutusTx.compile [||mkUntypedValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 params
