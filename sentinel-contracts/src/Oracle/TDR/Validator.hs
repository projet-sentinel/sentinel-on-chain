module Oracle.TDR.Validator
    (   mkValidator
    ,   mkUntypedValidator
    ,   compiledCode
    )   where

import           Data.Maybe                               (fromMaybe)
import           PlutusCore.Version                       (plcVersion100)
import           PlutusLedgerApi.V1.Address               (Address (..),
                                                           scriptHashAddress)
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
                                                           findOwnInput)
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
                                                           (&&), (==))
import qualified Prelude                                  as Haskell

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           Oracle.TDR.Types                         (TDRAction (..),
                                                           TDRDatum (..),
                                                           TDRParams (..),
                                                           TDRRefScriptDatum (..))
import           Oracle.ThreatDatabase.Types              (ThreatDatabaseAction (..),
                                                           ThreatDatabaseRefScriptDatum (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
mkValidator :: TDRParams -> TDRDatum -> TDRAction -> ScriptContext -> Bool
mkValidator TDRParams{..} datum redeemer ctx@ScriptContext{ scriptContextTxInfo = TxInfo{..} }
    |   ContractRefScriptDatum refData <- getTxOutInlineDatum (getScriptRefTxOut ownHash)
    ,   TDRRefScriptDatum tDRIndex treasuryAddress databaseIndex threatDatabaseSH <- unsafeFromBuiltinData refData
    ,   traceIfFalse "TDR 1" (builtinTDRIndex == tDRIndex)
        =
            case (redeemer, datum) of

                -----------------------------------------------------------------------------------------
                --                         ACCEPT REQUEST FOR UPDATE THREAT DATA                       --
                -----------------------------------------------------------------------------------------

                (AcceptRequest,
                    TDRDatum{} )
                        |   ContractRefScriptDatum refData' <- getTxOutInlineDatum (getScriptRefTxOut threatDatabaseSH)
                        ,   ThreatDatabaseRefScriptDatum databaseIndex' _ _ <- unsafeFromBuiltinData refData'
                        ,   UpdateThreatData <- getSHRedeemer threatDatabaseSH
                            ->
                                traceIfFalse "TDR 1.1"
                                    (databaseIndex == databaseIndex')

                -----------------------------------------------------------------------------------------
                --                                    REMOVE REQUEST                                   --
                -----------------------------------------------------------------------------------------

                (RemoveRequest,
                    TDRDatum requesterAddress _ _ _ )
                        ->
                                traceIfFalse "TDR 2.1"
                                    (txSignedByAddress requesterAddress)
                            &&  traceIfFalse "TDR 2.2"
                                    (isAddrGettingPaidExactly requesterAddress (adaAmountOf (txOutValue exactlyOneScriptInputUTxO)))


                -----------------------------------------------------------------------------------------
                --                                 GARBAGE COLLECTION ACTION                                  --
                -----------------------------------------------------------------------------------------

                -- (GarbageCollection, TDRDatum{}) ->
                --     traceIfFalse "TDR 3.1" False


                (GarbageCollection, _) ->
                    traceIfFalse "TDR 4.1"
                        $ isAddrGettingPaidExactly treasuryAddress (adaAmountOf (txOutValue exactlyOneScriptInputUTxO))


                -----------------------------------------------------------------------------------------
                --                                 TDR DEBUGGER ACTION                                 --
                -----------------------------------------------------------------------------------------

                (TDRDebugger, _) ->
                    traceIfFalse "TDR 5.1"
                        $ tDRDebuggerPKH  `elem` txInfoSignatories

                -----------------------------------------------------------------------------------------
                --                                   ANY OTHER CASE                                    --
                -----------------------------------------------------------------------------------------

                (_,_) -> traceIfFalse "TDR 4.1" False

    |   otherwise = traceError "TDR 2"

            where

                getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
                getTxOutInlineDatum tx
                    | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                    = unsafeFromBuiltinData @dat inline
                    | otherwise = traceError "TDR 3"

                valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
                valueOf (Value mp) cur tn
                    |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                    |   otherwise = 0

                singleScriptInputUTxO :: TxOut
                singleScriptInputUTxO
                    |   Just i      <-  findOwnInput ctx = txInInfoResolved i
                    |   otherwise   =   traceError "TDR 4"

                adaAmountOf :: Value -> Integer
                adaAmountOf val = valueOf val adaSymbol adaToken

                ownHashes :: TxOut -> ScriptHash
                ownHashes TxOut{txOutAddress=Address (ScriptCredential s) _} = s
                ownHashes _ = traceError "TDR 5"

                ownHash :: ScriptHash
                ownHash = ownHashes singleScriptInputUTxO

                getScriptRefTxOut :: ScriptHash -> TxOut
                getScriptRefTxOut sh
                        |   Just i <- find  (   \i ->
                                                    (   case txOutAddress (txInInfoResolved i) of
                                                            (Address (ScriptCredential sh') _) -> sh' == contractsControllerSH
                                                            _ -> False
                                                    )
                                                &&
                                                    fromJust (txOutReferenceScript (txInInfoResolved i)) == sh
                                                &&
                                                    valueOf (txOutValue (txInInfoResolved i)) currencySymbolOfCAT (scriptHashToTokenName sh) == 1
                                            )
                                            txInfoReferenceInputs = txInInfoResolved i
                        |   otherwise = traceError "TDR 6"
                            where

                                scriptHashToTokenName :: ScriptHash -> TokenName
                                scriptHashToTokenName (ScriptHash hash) = TokenName hash

                                fromJust :: Maybe a -> a
                                fromJust (Just x) = x
                                fromJust _        = traceError "TDR 7"

                isAddrGettingPaidExactly :: Address -> Integer -> Bool
                isAddrGettingPaidExactly addr int =
                    any (\o -> txOutAddress o == addr && adaAmountOf (txOutValue o) == int) txInfoOutputs

                txSignedByAddress :: Address -> Bool
                txSignedByAddress (Address (PubKeyCredential pkh) _) = pkh `elem` txInfoSignatories
                txSignedByAddress _ = traceError "TDR 8"

                exactlyOneScriptInputUTxO :: TxOut
                exactlyOneScriptInputUTxO
                    | [tx] <- filter (\tx -> txOutAddress tx == txOutAddress singleScriptInputUTxO) (map txInInfoResolved txInfoInputs) = tx
                    | otherwise = traceError "TDR 9"

                getSHRedeemer :: forall rad. (UnsafeFromData rad) => ScriptHash -> rad
                getSHRedeemer sh
                    |   Just i <- find (\i -> txOutAddress (txInInfoResolved i) == scriptHashAddress sh) txInfoInputs
                    ,   Just rawMintingRedeemer <- lookup (Spending (txInInfoOutRef i)) txInfoRedeemers
                    =   unsafeFromBuiltinData @rad $ getRedeemer rawMintingRedeemer
                    |   otherwise = traceError "TDR 10"


{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
-- |                                  COMPILATION                                     | --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: TDRParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedValidator params datum redeemer ctx
    |   mkValidator params (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx) = ()
    |   otherwise = error()

compiledCode :: TDRParams -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledCode params = case compiled of
    Right c -> c
    Left e  -> Haskell.error e
    where
        compiled = $$(PlutusTx.compile [||mkUntypedValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 params
