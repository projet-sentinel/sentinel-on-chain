module  Administration.ContractsController.Types
            (   ContractsControllerParams   (..)
            ,   ContractsControllerDatum    (..)
            ,   ContractsControllerAction   (..)
            )
                where

import           Data.Aeson                (ToJSON)
import           GHC.Generics              (Generic)
import           Ledger.Orphans            ()
import           PlutusLedgerApi.V1.Crypto (PubKeyHash)
import           PlutusLedgerApi.V1.Time   (POSIXTime)
import           PlutusLedgerApi.V2        (CurrencySymbol) 
import           PlutusTx                  (BuiltinData)
import qualified PlutusTx
import           PlutusTx.Prelude          (Bool (..), Eq, (&&), (==))
import qualified Prelude                   as Haskell


data ContractsControllerParams = ContractsControllerParams
    {   currencySymbolOfCAT              ::  CurrencySymbol
    ,   contractsControllerSHDebuggerPKH ::  PubKeyHash
    }
    deriving
        (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

PlutusTx.makeLift ''ContractsControllerParams


data ContractsControllerDatum
    =   ContractsControllerRefScriptDatum
            {   coreTeamCSs            :: [CurrencySymbol]
            ,   sentinelBoardCSs       :: [CurrencySymbol]
            ,   annualAssemblyDeadline :: POSIXTime
            }
    |   BoardMemberDatum
            {   boardMemberPKHs  :: [PubKeyHash]
            }
    |   ContractRefScriptDatum BuiltinData
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

instance Eq ContractsControllerDatum where
    {-# INLINABLE (==) #-}
    (==) :: ContractsControllerDatum -> ContractsControllerDatum -> Bool
    ContractsControllerRefScriptDatum a b c == ContractsControllerRefScriptDatum a' b' c'
        |   a == a' && b == b' && c == c' = True
    BoardMemberDatum a == BoardMemberDatum a'
        |   a == a' = True
    ContractRefScriptDatum a == ContractRefScriptDatum a'
        |   a == a' = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''ContractsControllerDatum   [ ( 'ContractsControllerRefScriptDatum, 1 )
                                                        , ( 'BoardMemberDatum,                  2 )
                                                        , ( 'ContractRefScriptDatum,            3 )
                                                        ]


data ContractsControllerAction
    =   ContractsControllerManagement
    |   BoardMemberAction
    |   AnnualAssemblyAction
    |   ContractsControllerDebugger
    deriving (Haskell.Show, Generic, ToJSON)

PlutusTx.makeIsDataIndexed ''ContractsControllerAction  [ ( 'ContractsControllerManagement, 1 )
                                                        , ( 'BoardMemberAction,             2 )
                                                        , ( 'AnnualAssemblyAction,          3 )
                                                        , ( 'ContractsControllerDebugger,   4 )
                                                        ]
