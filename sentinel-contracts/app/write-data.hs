{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import           Data.Aeson
import           Data.ByteString.Lazy                           as BSL (readFile)
import qualified Data.ByteString.Lazy                           as LBS
import           Data.Maybe                                     (fromJust)
import           Data.String                                    (fromString)
import           Data.Text                                      (pack)
import           System.Environment                             (getArgs)
import           Text.Printf                                    (printf)

import           Cardano.Api
import           Cardano.Api.Shelley                            (Address (..),
                                                                 PlutusScript (..),
                                                                 fromPlutusData,
                                                                 toPlutusData)
import           Cardano.Crypto.Hash.Class                      (hashToBytes)
import           Cardano.Ledger.BaseTypes                       as CLB
import           Cardano.Ledger.Credential                      as Ledger
import           Cardano.Ledger.Crypto                          (StandardCrypto)
import           Cardano.Ledger.Hashes                          (ScriptHash (..))
import           Cardano.Ledger.Keys                            (KeyHash (..))
import           PlutusLedgerApi.V1.Address                     as Plutus
import           PlutusLedgerApi.V1.Credential                  as Plutus
import           PlutusLedgerApi.V1.Crypto                      as Plutus
import           PlutusLedgerApi.V1.Time
import           PlutusLedgerApi.V2
import           PlutusLedgerApi.V2                             as Plutus
import qualified PlutusTx

import           Administration.BMAT.Policy                     as BMAT
import           Administration.CAT.Policy                      as CAT
import           Administration.CAT.Types                       as CAT
import           Administration.ContractsController.Types       as Controller
import           Administration.ContractsController.Validator   as Controller
import           Oracle.TDAT.AddressList.Policy                 as TDATAddressList
import           Oracle.TDAT.ScriptHashList.Policy              as TDATSHList
import           Oracle.TDAT.Types                              as TDAT
import           Oracle.ThreatDatabase.AddressList.Validator    as ThreatDatabaseAddressList
import           Oracle.ThreatDatabase.ScriptHashList.Validator as ThreatDatabaseSHList
import           Oracle.ThreatDatabase.Types                    as ThreatDatabase
import           Users.Escrow.Types                             as Escrow
import           Users.Escrow.Validator                         as Escrow

main :: IO ()
main = do
    args <- getArgs
    case head args of
        "Validator" -> do
            case drop 1 args of
                ["Controller", file, catCs', debaddr'] -> do
                    printf "file: %s\n" file
                    writeValidator (File file) (Controller.compiledCode $ ContractsControllerParams (fromString catCs') (pkhFromAddress debaddr'))
                ["ThreatDatabaseAddrList", file, dbIndex', controllerSh', catCs', datCs', debaddr'] -> do
                    printf "file: %s\n" file
                    writeValidator (File file) (ThreatDatabaseAddressList.compiledCode $ ThreatDatabaseParams (fromString dbIndex') (fromString controllerSh') (fromString catCs') (fromString datCs') (pkhFromAddress debaddr'))
                ["ThreatDatabaseSHList", file, dbIndex', controllerSh', catCs', datCs', debaddr'] -> do
                    printf "file: %s\n" file
                    writeValidator (File file) (ThreatDatabaseSHList.compiledCode $ ThreatDatabaseParams (fromString dbIndex') (fromString controllerSh') (fromString catCs') (fromString datCs') (pkhFromAddress debaddr'))
                ["Escrow", file, tdatCs', tdAddr'] -> do
                    printf "file: %s\n" file
                    writeValidator (File file) (Escrow.compiledCode $ EscrowParams (fromString tdatCs') (fromJust $ tryReadAddress tdAddr'))
                _ -> error "validator not found"
        "Policy" -> do
            case drop 1 args of
                ("BMAT":xs) -> do
                    let file = head xs
                        txId = fromString (xs!!1)
                        txIdx = read (xs!!2)
                        boardMemberPkhs = map pkhFromAddress (drop 3 xs)
                    printf "file: %s\n" file
                    writePolicy (File file) (BMAT.compiledCode $ BMATParams boardMemberPkhs txId txIdx)
                ["CAT", file, deadline', debaddr'] -> do
                    printf "file: %s\n" file
                    writePolicy (File file) (CAT.compiledCode $ CATParams (fromMilliSeconds $ DiffMilliSeconds (read deadline')) (pkhFromAddress debaddr'))
                ["TDATAddrList", file, dbIndex', controllerSh', catCs', debaddr'] -> do
                    printf "file: %s\n" file
                    writePolicy (File file) (TDATAddressList.compiledCode $ TDATParams (fromString dbIndex') (fromString controllerSh') (fromString catCs') (pkhFromAddress debaddr'))
                ["TDATSHList", file, dbIndex', controllerSh', catCs', debaddr'] -> do
                    printf "file: %s\n" file
                    writePolicy (File file) (TDATSHList.compiledCode $ TDATParams (fromString dbIndex') (fromString controllerSh') (fromString catCs') (pkhFromAddress debaddr'))
                _ -> error "policy not found"
        "Redeemer" -> do
            case drop 1 args of
                ["Unit", file] -> do
                    printf "file: %s\n" file
                    writeJson file ()
                ("CAT":xs) -> do
                    case xs of
                        ["SentinelBoardInception", file] -> do
                            printf "file: %s\n" file
                            writeJson file SentinelBoardInception
                        ["SentinelBoardAction", file] -> do
                            printf "file: %s\n" file
                            writeJson file SentinelBoardAction
                        ["CATDebugger", file] -> do
                            printf "file: %s\n" file
                            writeJson file CATDebugger
                        _ -> error "CAT redeemer not found"
                ("Controller":xs) -> do
                    case xs of
                        ["ContractsControllerManagement", file] -> do
                            printf "file: %s\n" file
                            writeJson file ContractsControllerManagement
                        ["BoardMemberAction", file] -> do
                            printf "file: %s\n" file
                            writeJson file BoardMemberAction
                        ["AnnualAssemblyAction", file] -> do
                            printf "file: %s\n" file
                            writeJson file AnnualAssemblyAction
                        ["ContractsControllerDebugger", file] -> do
                            printf "file: %s\n" file
                            writeJson file ContractsControllerDebugger
                        _ -> error "Controller redeemer not found"
                ("TDAT":xs) -> do
                    case xs of
                        ["MintTDAT", file] -> do
                            printf "file: %s\n" file
                            writeJson file MintTDAT
                        ["BurnTDAT", file] -> do
                            printf "file: %s\n" file
                            writeJson file BurnTDAT
                        ["TDATDebugger", file] -> do
                            printf "file: %s\n" file
                            writeJson file TDATDebugger
                        _ -> error "TDAT redeemer not found"
                ("ThreatDatabase":xs) -> do
                    case xs of
                        ["UpdateThreatData", file] -> do
                            printf "file: %s\n" file
                            writeJson file UpdateThreatData
                        ["RemoveThreatData", file] -> do
                            printf "file: %s\n" file
                            writeJson file RemoveThreatData
                        ["ThreatDatabaseDebugger", file] -> do
                            printf "file: %s\n" file
                            writeJson file ThreatDatabaseDebugger
                        _ -> error "ThreatDatabase redeemer not found"
                _ -> error "redeemer not found"
        "Datum" -> do
            case drop 1 args of
                ("Controller":xs) -> do
                    case xs of
                        ("ContractsControllerRefScriptDatum":xs') -> do
                            let file = head xs'
                                time = fromMilliSeconds $ DiffMilliSeconds (read (xs'!!1))
                                teamCss = map fromString (drop 2 xs')
                            writeJson file (ContractsControllerRefScriptDatum (take 3 teamCss) (drop 3 teamCss) time)
                        ("BoardMemberDatum":xs') -> do
                            let file = head xs'
                                boardMemberPkhs = map (\x -> pkhFromAddress x) (drop 1 xs')
                            printf "file: %s\n" file
                            writeJson file (BoardMemberDatum boardMemberPkhs)
                        ("ContractRefScriptDatum":xs'') -> do
                            case xs'' of
                                ["ThreatDatabase", file, tdrsdFile] -> do
                                    Just tdrsd <- readJSON @ThreatDatabaseRefScriptDatum tdrsdFile
                                    printf "file: %s\n" file
                                    writeJson file (ContractRefScriptDatum $ toBuiltinData tdrsd)
                                ["TDAT", file, tdrsdFile] -> do
                                    Just tdrsd <- readJSON @TDATRefScriptDatum tdrsdFile
                                    printf "file: %s\n" file
                                    writeJson file (ContractRefScriptDatum $ toBuiltinData tdrsd)
                                _ -> error "ContractRefScriptDatum not found"
                        _ -> error "Controller datum not found"
                ("ThreatDatabaseAddrListDatum":xs') -> do
                    let file = head xs'
                        tag = read (xs'!!1)
                        score = read (xs'!!2)
                        updated = fromMilliSeconds $ DiffMilliSeconds (read (xs'!!3))
                        addrs = map (fromJust . tryReadAddress) (drop 4 xs')
                    writeJson file (ThreatDatabaseAddrListDatum tag score addrs updated)
                ("ThreatDatabaseRefScriptDatum":xs') -> do
                    let file = head xs'
                        treasuryAddr = fromJust $ tryReadAddress (xs'!!2)
                        adminPkhs = map pkhFromAddress (drop 3 xs')
                    writeJson file (ThreatDatabaseRefScriptDatum (fromString (xs'!!1)) adminPkhs treasuryAddr)
                ("TDATRefScriptDatum":xs') -> do
                    let file = head xs'
                        tdatAddr = fromJust $ tryReadAddress (xs'!!2)
                        adminPkhs = map pkhFromAddress (drop 3 xs')
                    writeJson file (TDATRefScriptDatum (fromString (xs'!!1)) adminPkhs tdatAddr)
                ["Escrow", file, senderaddr', receiveraddr'] -> do
                    writeJson file (EscrowDatum (fromJust $ tryReadAddress senderaddr') (fromJust $ tryReadAddress receiveraddr'))
                _ -> error "datum not found"
        _ -> error "command not found"

writeCBORValidator :: File content 'Out -> SerialisedScript -> IO (Either (FileError ()) ())
writeCBORValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised

writePolicy :: File content 'Out -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())-> IO ()
writePolicy file c = do
    g <- writeCBORValidator file (serialiseCompiledCode c)
    case g of
        Right s -> return s
        Left _  -> error "compiling"

writeValidator :: File content 'Out -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())-> IO ()
writeValidator file c = do
    g <- writeCBORValidator file (serialiseCompiledCode c)
    case g of
        Right s -> return s
        Left _  -> error "compiling"

readJSON :: FromData a => FilePath -> IO (Maybe a)
readJSON file = do
    fileContent <- BSL.readFile file
    let fc = fromJust $ decode fileContent
    case scriptDataFromJson ScriptDataJsonDetailedSchema fc of
        Right d -> return (fromData (toPlutusData $ getScriptData d))
        Left _  -> error "readJSON"

writeJson :: ToData a => FilePath -> a -> IO ()
writeJson file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusTx.toData

credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (Cardano.Ledger.Hashes.ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ScriptHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h

stakeReferenceLedgerToPlutus :: Ledger.StakeReference StandardCrypto -> Maybe Plutus.StakingCredential
stakeReferenceLedgerToPlutus (StakeRefBase x)                   = Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (StakeRefPtr (Ptr (SlotNo x) (CLB.TxIx y) (CertIx z))) = Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus StakeRefNull                       = Nothing

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ pack x of
    Nothing                                      -> Nothing
    Just (AddressByron _)                        -> Nothing
    Just (AddressShelley (ShelleyAddress _ p s)) -> Just Plutus.Address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }

pkhFromAddress :: String -> PubKeyHash
pkhFromAddress x = case tryReadAddress x of
    Nothing   ->    error "address reading error"
    Just addr ->    fromJust $ Plutus.toPubKeyHash addr
