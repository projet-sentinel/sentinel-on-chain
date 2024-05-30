module Users.Escrow.Validator
    (   mkValidator
    ,   mkUntypedValidator
    ,   compiledCode
    )   where

import           Data.Maybe                    (fromMaybe)
import           PlutusCore.Version            (plcVersion100)
import           PlutusLedgerApi.V1.Address    (Address (..))
import           PlutusLedgerApi.V1.Credential (Credential (..))
import           PlutusLedgerApi.V1.Scripts    (Datum (..))
import           PlutusLedgerApi.V1.Value      (CurrencySymbol (..),
                                                TokenName (..), Value (..),
                                                adaSymbol, adaToken, symbols)
import           PlutusLedgerApi.V2.Contexts   (ScriptContext (..),
                                                TxInInfo (..), TxInfo (..),
                                                TxOut (..), findOwnInput)
import           PlutusLedgerApi.V2.Tx         (OutputDatum (..))
import           PlutusTx                      (BuiltinData, CompiledCode,
                                                applyCode, compile, liftCode,
                                                unsafeFromBuiltinData)
import           PlutusTx.AssocMap             (lookup)
import           PlutusTx.Prelude              (Bool (..), Either (..), Integer,
                                                Maybe (..), any, elem, error,
                                                find, map, not, otherwise,
                                                traceError, traceIfFalse, (&&),
                                                (==), (>), (||))
import qualified Prelude                       as Haskell

import           Oracle.ThreatDatabase.Types   (ThreatDatabaseAddrListDatum (..))
import           Users.Escrow.Types            (EscrowDatum (..),
                                                EscrowParams (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
{-|
    The `mkValidator` function is the core of the Escrow validator logic. It takes an `EscrowParams` record, an `EscrowDatum`, and a `ScriptContext` as input, and returns a boolean indicating whether the transaction is valid or not.

    The function performs the following checks:
        1. Ensures that the transaction output is being paid to either the sender or the receiver address exactly.
        2. Ensures that the transaction is signed by either the sender or the receiver address.
        3. If the threat score is greater than 5, it ensures that none of the addresses in the threat data are getting paid.

    The function also includes several helper functions:
        - `getTxOutInlineDatum`: Extracts the `ThreatDatabaseAddrListDatum` from a given `TxOut`.
        - `getThreatDatabaseUtxOTxOut`: Finds the `TxOut` that contains the threat database information.
        - `valueOf`: Retrieves the value of a specific token in a `Value`.
        - `singleScriptInputUTxO`: Finds the single script input `TxOut`.
        - `adaAmountOf`: Calculates the ADA amount in a `Value`.
        - `isAddrGettingPaidExactly`: Checks if a specific address is getting paid the exact amount.
        - `txSignedByAddress`: Checks if the transaction is signed by a specific address.

    Parameters:
        - `EscrowParams`: A record containing the necessary parameters for the Escrow validator.
        - `EscrowDatum`: The datum for the Escrow script.
        - `ScriptContext`: The script context for the current transaction.

    Example usage:
        mkValidator
            EscrowParams
                { currencySymbolOfTDAT = "05514ec851b6f7ca902dd88dd5a78444892e60d06746f63dc79e4877"
                , threatDatabaseAddress = "addr_test1wz6pk0g5xtvmdm9n3v58emvnwam0aswqstademgatk396xcrex36g"
                }
            ()
            (ScriptContext { scriptContextTxInfo = TxInfo { ... } })

    Returns:
        - `Bool`: `True` if the transaction is valid, `False` otherwise.

    Note:
        - The function makes extensive use of pattern matching and error handling to ensure that the validation logic is robust and handles various edge cases.
-}
mkValidator :: EscrowParams -> EscrowDatum -> () -> ScriptContext -> Bool
mkValidator EscrowParams{..} datum _ ctx@ScriptContext{ scriptContextTxInfo = TxInfo{..} } =
    case datum of
        (EscrowDatum senderAddress receiverAddress) ->
            case getTxOutInlineDatum getThreatDatabaseUtxOTxOut of
                ThreatDatabaseAddrListDatum _ threatScore threatData _ ->
                    let amount = adaAmountOf (txOutValue singleScriptInputUTxO)
                    in      traceIfFalse "Escrow 1.1"
                                (       isAddrGettingPaidExactly receiverAddress amount
                                    ||
                                        isAddrGettingPaidExactly senderAddress amount
                                )
                        &&  traceIfFalse "Escrow 1.2"
                                (       txSignedByAddress receiverAddress
                                    ||
                                        txSignedByAddress senderAddress
                                )
                        &&  traceIfFalse "Escrow 1.3"
                                (   if threatScore > 5
                                    then traceIfFalse "Escrow 1.4"
                                        (not (any (`isAddrGettingPaidExactly` amount) threatData))
                                    else
                                        traceIfFalse "Escrow 1.5" True
                                )

        where

            getTxOutInlineDatum :: TxOut -> ThreatDatabaseAddrListDatum
            getTxOutInlineDatum tx
                | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                = unsafeFromBuiltinData @ThreatDatabaseAddrListDatum inline
                | otherwise = traceError "Escrow 1"

            getThreatDatabaseUtxOTxOut :: TxOut
            getThreatDatabaseUtxOTxOut
                |   Just i <- find (\i -> isCurrencySymbolExist i && (txOutAddress i == threatDatabaseAddress)) (getAllInputsTxOuts txInfoReferenceInputs) = i
                |   otherwise = traceError "Escrow 2"
                    where
                        getAllInputsTxOuts :: [TxInInfo] -> [TxOut]
                        getAllInputsTxOuts inputs = map txInInfoResolved inputs

                        isCurrencySymbolExist :: TxOut -> Bool
                        isCurrencySymbolExist tx = any (== currencySymbolOfTDAT) (symbols (txOutValue tx))


            valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
            valueOf (Value mp) cur tn
                |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                |   otherwise = 0

            singleScriptInputUTxO :: TxOut
            singleScriptInputUTxO
                |   Just i      <-  findOwnInput ctx = txInInfoResolved i
                |   otherwise   =   traceError "Escrow 3"

            adaAmountOf :: Value -> Integer
            adaAmountOf val = valueOf val adaSymbol adaToken

            isAddrGettingPaidExactly :: Address -> Integer -> Bool
            isAddrGettingPaidExactly addr int =
                any (\o -> txOutAddress o == addr && adaAmountOf (txOutValue o) == int) txInfoOutputs

            txSignedByAddress :: Address -> Bool
            txSignedByAddress (Address (PubKeyCredential pkh) _) = pkh `elem` txInfoSignatories
            txSignedByAddress _ = traceError "Escrow 4"

{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
-- |                                  COMPILATION                                     | --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: EscrowParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedValidator params datum redeemer ctx
    |   mkValidator params (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx) = ()
    |   otherwise = error()

compiledCode :: EscrowParams -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledCode params = case compiled of
    Right c -> c
    Left e  -> Haskell.error e
    where
        compiled = $$(PlutusTx.compile [||mkUntypedValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 params
