module  Administration.CAT.Policy
            (   mkPolicy
            ,   mkUntypedPolicy
            ,   compiledCode
            )
                where

import           PlutusCore.Version                       (plcVersion100)
import           PlutusLedgerApi.V1.Interval              (Extended (..),
                                                           Interval (..),
                                                           UpperBound (..),
                                                           contains, to)
import           PlutusLedgerApi.V1.Scripts               (Datum (..))
import           PlutusLedgerApi.V1.Time                  (POSIXTime)
import           PlutusLedgerApi.V1.Value                 (flattenValue,
                                                           valueOf)
import           PlutusLedgerApi.V2.Contexts              (ScriptContext (..),
                                                           TxInInfo (..),
                                                           TxInfo (..),
                                                           TxOut (..),
                                                           ownCurrencySymbol)
import           PlutusLedgerApi.V2.Tx                    (OutputDatum (..))
import           PlutusLedgerApi.V2                       (CurrencySymbol (..),
                                                           TokenName (..))
import           PlutusTx                                 (BuiltinData,
                                                           CompiledCode,
                                                           applyCode, compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.Builtins                        (divideInteger,
                                                           emptyByteString)
import           PlutusTx.Prelude                         (Bool (..),
                                                           Either (..),
                                                           Maybe (..), all, any,
                                                           elem, error, filter,
                                                           find, id, length,
                                                           map, not, otherwise,
                                                           traceError,
                                                           traceIfFalse, ($),
                                                           (&&), (+), (<), (<=),
                                                           (==), (>=))
import qualified Prelude                                  as Haskell

import           Administration.CAT.Types                 (CATAction (..),
                                                           CATParams (..))
import           Administration.ContractsController.Types (ContractsControllerDatum (..))

{-==============================================================================================================================-}
{-==========                                          MINTING POLICY SECTION                                          ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkPolicy #-}
mkPolicy :: CATParams -> CATAction -> ScriptContext -> Bool
mkPolicy CATParams{..} redeemer ctx@ScriptContext{ scriptContextTxInfo = TxInfo{..}} =

    case redeemer of

            -----------------------------------------------------------------------------------------
            --                              SENTINEL BOARD INCEPTION                               --
            -----------------------------------------------------------------------------------------

            SentinelBoardInception
                |   ContractsControllerRefScriptDatum coreTeamCSs sentinelBoard annualAssemblyDeadline <- getTxOutInlineDatum getSentinelBoardTxOut -> 
                            traceIfFalse "CAT 1.1"
                                (to initializationDeadline `contains` txInfoValidRange)
                        &&  traceIfFalse "CAT 1.2"
                                (all isBoardMemberCSPresent sentinelBoard)
                        &&  traceIfFalse "CAT 1.3"
                                (all isBoardMemberCSPresent coreTeamCSs)
                        &&  traceIfFalse "CAT 1.4"
                                (getCurrentTime + 31536000000 <= annualAssemblyDeadline)
                        &&  traceIfFalse "CAT 1.5"
                                wasMintedExactly

            -----------------------------------------------------------------------------------------
            --                                SENTINEL BOARD ACTION                                --
            -----------------------------------------------------------------------------------------

            SentinelBoardAction
                |   ContractsControllerRefScriptDatum coreTeamCSs sentinelBoard annualAssemblyDeadline <- getTxOutInlineDatum getSentinelBoardRefUTxO ->
                            traceIfFalse "CAT 2.1"
                                (didSentinelBoardMembersSinged sentinelBoard)
                        &&  traceIfFalse "CAT 2.2"
                                (any isBoardMemberCSPresent coreTeamCSs)
                        &&  traceIfFalse "CAT 2.3"
                                (not isEmptyTokenMinting)
                        &&  traceIfFalse "CAT 2.4"
                                (to annualAssemblyDeadline `contains` txInfoValidRange)

            -----------------------------------------------------------------------------------------
            --                              CAT MINTER DEBUGGER ACTION                             --
            -----------------------------------------------------------------------------------------

            CATDebugger ->
                traceIfFalse "CAT 3.1"
                    $ cATDebuggerPKH `elem` txInfoSignatories

            -----------------------------------------------------------------------------------------
            --                                   ANY OTHER CASE                                    --
            -----------------------------------------------------------------------------------------

            _ -> traceIfFalse "CAT 1" False

            where

                emptyTokenName :: TokenName
                emptyTokenName = TokenName emptyByteString

                getSentinelBoardTxOut :: TxOut
                getSentinelBoardTxOut
                    |   Just o <- find (\ o ->
                                            valueOf (txOutValue o) (ownCurrencySymbol ctx) emptyTokenName == 1)
                                            txInfoOutputs = o
                    |   otherwise = traceError "CAT 2"

                getSentinelBoardRefUTxO :: TxOut
                getSentinelBoardRefUTxO
                        |   Just i <- find  (   \i ->
                                                    valueOf (txOutValue (txInInfoResolved i)) (ownCurrencySymbol ctx) emptyTokenName == 1
                                            )
                                            txInfoReferenceInputs = txInInfoResolved i
                        |   otherwise = traceError "CAT 3"

                getTxOutInlineDatum :: TxOut -> ContractsControllerDatum
                getTxOutInlineDatum tx
                    | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                    = unsafeFromBuiltinData @ContractsControllerDatum inline
                    | otherwise = traceError "CAT 4"

                isBoardMemberCSPresent :: CurrencySymbol -> Bool
                isBoardMemberCSPresent cs
                    |   Just i <- find (\i -> valueOf (txOutValue (txInInfoResolved i)) cs emptyTokenName == 1) txInfoReferenceInputs
                    ,   BoardMemberDatum boardMemberPKHs <- getTxOutInlineDatum (txInInfoResolved i)
                    =   any (`elem` txInfoSignatories) boardMemberPKHs
                    |   otherwise = False

                didSentinelBoardMembersSinged ::  [CurrencySymbol] -> Bool
                didSentinelBoardMembersSinged [] = False
                didSentinelBoardMembersSinged csList
                    |   length csList < 3 = all isBoardMemberCSPresent csList
                    |   length csList >= 3 =
                            let numOfSignatories = length (filter id (map isBoardMemberCSPresent csList))
                            in  numOfSignatories >= (length csList `divideInteger` 2) + 1
                    |   otherwise = False

                wasMintedExactly ::  Bool
                wasMintedExactly =
                        length (filter (\(cs,tn,amt) -> cs == ownCurrencySymbol ctx && amt == 1 && tn == emptyTokenName) (flattenValue txInfoMint)) == 1
                    &&  length (flattenValue txInfoMint) == 1

                isEmptyTokenMinting ::  Bool
                isEmptyTokenMinting =
                     any (\(cs,tn,_) -> cs == ownCurrencySymbol ctx && tn == emptyTokenName) (flattenValue txInfoMint)

                getCurrentTime :: POSIXTime
                getCurrentTime =
                    case ivTo txInfoValidRange of
                        UpperBound (Finite t) _ -> t
                        _                       -> traceError "CAT 4"

                -- getCurrentTime_2 :: POSIXTime
                -- getCurrentTime_2 =
                --     case ivFrom txInfoValidRange of
                --         LowerBound (Finite t) _ -> t
                --         _                       -> traceError "CAT 4"

{-============================================== END OF MINTING POLICY SECTION =================================================-}

{-# INLINABLE mkUntypedPolicy #-}
mkUntypedPolicy :: CATParams -> BuiltinData -> BuiltinData -> ()
mkUntypedPolicy params redeemer ctx
    |   mkPolicy params (PlutusTx.unsafeFromBuiltinData redeemer) (PlutusTx.unsafeFromBuiltinData ctx) = ()
    |   otherwise = error()

compiledCode :: CATParams -> CompiledCode (BuiltinData -> BuiltinData -> ())
compiledCode params = case compiled of
    Right c -> c
    Left e  -> Haskell.error e
    where
        compiled = $$(PlutusTx.compile [||mkUntypedPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 params
