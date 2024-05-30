module Administration.ContractsController.Validator
    (   mkValidator
    ,   mkUntypedValidator
    ,   compiledCode
    )   where

import           Data.Maybe                               (fromMaybe)
import           PlutusCore.Version                       (plcVersion100)
import           PlutusLedgerApi.V1.Address               (Address (..))
import           PlutusLedgerApi.V1.Credential            (Credential (..))
import           PlutusLedgerApi.V1.Interval              (contains, from, to)
import           PlutusLedgerApi.V1.Scripts               (Datum (..))
import           PlutusLedgerApi.V1.Value                 (flattenValue)
import           PlutusLedgerApi.V2.Contexts              (ScriptContext (..),
                                                           TxInInfo (..),
                                                           TxInfo (..),
                                                           TxOut (..),
                                                           findOwnInput,
                                                           getContinuingOutputs)
import           PlutusLedgerApi.V2                       (CurrencySymbol (..),
                                                           TokenName (..),
                                                           Value (..), 
                                                           ScriptHash (..))                                                        
import           PlutusLedgerApi.V2.Tx                    (OutputDatum (..))
import           PlutusTx                                 (BuiltinData,
                                                           CompiledCode,
                                                           applyCode, compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.AssocMap                        (lookup)
import           PlutusTx.Builtins                        (divideInteger,
                                                           emptyByteString)
import           PlutusTx.Prelude                         (Bool (..),
                                                           Either (..), Integer,
                                                           Maybe (..), all, any,
                                                           elem, error, filter,
                                                           find, id, length,
                                                           map, not, null,
                                                           otherwise,
                                                           traceError,
                                                           traceIfFalse, ($),
                                                           (&&), (+), (<), (<=),
                                                           (==), (>=))
import qualified Prelude                                  as Haskell

import           Administration.ContractsController.Types (ContractsControllerAction (..),
                                                           ContractsControllerDatum (..),
                                                           ContractsControllerParams (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
mkValidator :: ContractsControllerParams -> ContractsControllerDatum -> ContractsControllerAction -> ScriptContext -> Bool
mkValidator ContractsControllerParams{..} datum redeemer ctx@ScriptContext{ scriptContextTxInfo = TxInfo{..} }
    |   ContractsControllerRefScriptDatum coreTeamCSs sentinelBoardCSs annualAssemblyDeadline <- getTxOutInlineDatum getScriptRefTxOut
        =
            case (redeemer, datum) of

                -----------------------------------------------------------------------------------------
                --                            CONTRACTS CONTROLLER MANAGEMENT                          --
                -----------------------------------------------------------------------------------------

                (ContractsControllerManagement, dat) ->
                    case dat of
                        ContractsControllerRefScriptDatum {}
                            |   ContractsControllerRefScriptDatum coreTeamCSs' sentinelBoardCSs' annualAssemblyDeadline' <- getTxOutInlineDatum singleScriptOutputUTxO
                                ->
                                    traceIfFalse "ContractsController 1.1"
                                        (didSentinelBoardMembersSinged sentinelBoardCSs)
                                &&  traceIfFalse "ContractsController 1.2"
                                        (any isBoardMemberCSPresent coreTeamCSs)
                                &&  traceIfFalse "ContractsController 1.3"
                                        (valueOf (txOutValue singleScriptOutputUTxO) currencySymbolOfCAT emptyTokenName == 1)
                                &&  traceIfFalse "ContractsController 1.4"
                                        (not (null sentinelBoardCSs'))
                                &&  traceIfFalse "ContractsController 1.5"
                                        (coreTeamCSs == coreTeamCSs')
                                &&  traceIfFalse "ContractsController 1.6"
                                        (annualAssemblyDeadline == annualAssemblyDeadline')
                                &&  traceIfFalse "ContractsController 1.7"
                                        (to annualAssemblyDeadline `contains` txInfoValidRange)

                        ContractRefScriptDatum {} ->
                                traceIfFalse "ContractsController 1.8"
                                    (didSentinelBoardMembersSinged sentinelBoardCSs)
                            &&  traceIfFalse "ContractsController 1.9"
                                    (any isBoardMemberCSPresent coreTeamCSs)

                        _ -> False

                -----------------------------------------------------------------------------------------
                --                         CONTRACTS BOARD MEMBER ACTION                               --
                -----------------------------------------------------------------------------------------

                (BoardMemberAction,
                    BoardMemberDatum boardMemberPKHs) ->
                            traceIfFalse "ContractsController 2.1"
                                (isMember sentinelBoardCSs)
                        &&  traceIfFalse "ContractsController 2.2"
                                (any (`elem` txInfoSignatories) boardMemberPKHs)

                -----------------------------------------------------------------------------------------
                --                                 ANNUAL ASSEMBLY ACTION                              --
                -----------------------------------------------------------------------------------------

                (AnnualAssemblyAction, ContractsControllerRefScriptDatum {})
                    |   ContractsControllerRefScriptDatum coreTeamCSs' sentinelBoardCSs' annualAssemblyDeadline' <- getTxOutInlineDatum singleScriptOutputUTxO
                        ->
                                traceIfFalse "ContractsController 1.1"
                                    (didSentinelBoardMembersSinged sentinelBoardCSs)
                            &&  traceIfFalse "ContractsController 1.2"
                                    (any isBoardMemberCSPresent coreTeamCSs)
                            &&  traceIfFalse "ContractsController 1.3"
                                    (all isBoardMemberCSPresent sentinelBoardCSs')
                            &&  traceIfFalse "ContractsController 1.4"
                                    (all isBoardMemberCSPresent coreTeamCSs')
                            &&  traceIfFalse "ContractsController 1.5"
                                    (valueOf (txOutValue singleScriptOutputUTxO) currencySymbolOfCAT emptyTokenName == 1)
                            &&  traceIfFalse "ContractsController 1.6"
                                    (not (null sentinelBoardCSs'))
                            &&  traceIfFalse "ContractsController 1.7"
                                    (not (null coreTeamCSs'))
                            &&  traceIfFalse "ContractsController 1.8"
                                    (annualAssemblyDeadline + 31536000000 <= annualAssemblyDeadline')
                            &&  traceIfFalse "ContractsController 1.9"
                                    (from annualAssemblyDeadline `contains` txInfoValidRange)
                            &&  traceIfFalse "ContractsController 1.10"
                                    (isScriptCBORExistInTxOut singleScriptOutputUTxO)

                -----------------------------------------------------------------------------------------
                --                        CONTRACTS CONTROLLER DEBUGGER ACTION                         --
                -----------------------------------------------------------------------------------------

                (ContractsControllerDebugger, _) ->
                    traceIfFalse "ContractsController 3.1"
                        $ contractsControllerSHDebuggerPKH  `elem` txInfoSignatories

                -----------------------------------------------------------------------------------------
                --                                   ANY OTHER CASE                                    --
                -----------------------------------------------------------------------------------------

                (_,_) -> traceIfFalse "ContractsController 4.1" False

    |   otherwise = traceError "ContractsController 1"

            where

                valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
                valueOf (Value mp) cur tn
                    |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                    |   otherwise = 0

                emptyTokenName :: TokenName
                emptyTokenName = TokenName emptyByteString

                singleScriptInputUTxO :: TxOut
                singleScriptInputUTxO
                    |   Just i      <-  findOwnInput ctx = txInInfoResolved i
                    |   otherwise   =   traceError "ContractsController 2"

                singleScriptOutputUTxO :: TxOut
                singleScriptOutputUTxO
                    |   [o]         <-  getContinuingOutputs ctx = o
                    |   otherwise   =   traceError "ContractsController 3"

                ownHashes :: TxOut -> ScriptHash
                ownHashes TxOut{txOutAddress=Address (ScriptCredential s) _} = s
                ownHashes _ = traceError "ContractsController 4"

                ownHash :: ScriptHash
                ownHash = ownHashes singleScriptInputUTxO

                isOwnSH :: Address -> Bool
                isOwnSH (Address (ScriptCredential sh) _) = sh == ownHash
                isOwnSH _                                 = False

                isOwnRefScript :: Maybe ScriptHash -> Bool
                isOwnRefScript (Just sh) = sh == ownHash
                isOwnRefScript _         = False

                getScriptRefTxOut :: TxOut
                getScriptRefTxOut
                    |   Just i <- find (    \i ->

                                                    isOwnSH (txOutAddress (txInInfoResolved i))
                                                &&
                                                    valueOf (txOutValue (txInInfoResolved i)) currencySymbolOfCAT emptyTokenName == 1
                                                &&
                                                    isOwnRefScript (txOutReferenceScript (txInInfoResolved i))
                                        )
                                        txInfoReferenceInputs = txInInfoResolved i
                    |   otherwise = traceError "ContractsController 5"

                isScriptCBORExistInTxOut :: TxOut -> Bool
                isScriptCBORExistInTxOut tx =
                    case txOutReferenceScript tx of
                        Nothing -> traceError "ContractsController 6"
                        Just sh -> sh == ownHash

                getTxOutInlineDatum :: TxOut -> ContractsControllerDatum
                getTxOutInlineDatum tx
                    |   ( OutputDatum ( Datum inline )) <- txOutDatum tx
                    =   unsafeFromBuiltinData @ContractsControllerDatum inline
                    |   otherwise = traceError "ContractsController 7"


                isBoardMemberCSPresent :: CurrencySymbol -> Bool
                isBoardMemberCSPresent cs
                    |   Just i <- find (\i ->       valueOf (txOutValue (txInInfoResolved i)) cs emptyTokenName == 1
                                                &&
                                                    isOwnSH (txOutAddress (txInInfoResolved i))

                                        ) txInfoReferenceInputs
                    ,   BoardMemberDatum boardMemberPKHs <- getTxOutInlineDatum (txInInfoResolved i)
                    =   any (`elem` txInfoSignatories) boardMemberPKHs
                    |   otherwise = False

                didSentinelBoardMembersSinged ::  [CurrencySymbol] -> Bool
                didSentinelBoardMembersSinged [] = False
                didSentinelBoardMembersSinged csList
                    |   length csList < 3 = all isBoardMemberCSPresent csList
                    |   length csList >= 3 =
                            let numOfSignatories = length (filter id (map isBoardMemberCSPresent csList))
                            in numOfSignatories >= (length csList `divideInteger` 2) + 1
                    |   otherwise = False

                isMember ::  [CurrencySymbol] -> Bool
                isMember [] = False
                isMember csList =
                    any (\(cs,_,_) -> cs `elem` csList) (flattenValue (txOutValue singleScriptInputUTxO))


{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
-- |                                  COMPILATION                                     | --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: ContractsControllerParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedValidator params datum redeemer ctx
    |   mkValidator params (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx) = ()
    |   otherwise = error()

compiledCode :: ContractsControllerParams -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledCode params = case compiled of
    Right c -> c
    Left e  -> Haskell.error e
    where
        compiled = $$(PlutusTx.compile [||mkUntypedValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 params
