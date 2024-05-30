module  Administration.CAT.Types
            (   CATParams  (..)
            ,   CATAction  (..)
            )
                where

import           Data.Aeson                (ToJSON)
import           GHC.Generics              (Generic)
import           Ledger.Orphans            ()
import           PlutusLedgerApi.V1.Crypto (PubKeyHash)
import           PlutusLedgerApi.V1.Time   (POSIXTime)
import qualified PlutusTx
import qualified Prelude                   as Haskell


data CATParams = CATParams
    {   initializationDeadline ::  POSIXTime
    ,   cATDebuggerPKH         ::  PubKeyHash
    }
    deriving
        (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, ToJSON)

PlutusTx.makeLift ''CATParams

data CATAction
    =   SentinelBoardInception
    |   SentinelBoardAction
    |   CATDebugger
    deriving (Haskell.Show, Generic, ToJSON)


PlutusTx.makeIsDataIndexed ''CATAction  [ ( 'SentinelBoardInception,    1 )
                                        , ( 'SentinelBoardAction,       2 )
                                        , ( 'CATDebugger,               3 )
                                        ]
