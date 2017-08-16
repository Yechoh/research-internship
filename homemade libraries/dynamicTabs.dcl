definition module dynamicTabs

import iTasks
import qualified Data.Map as DM

dynamicTabs :: (Shared (Map k e)) (k -> String) (k (Shared e) -> Task ()) e -> Task [(TaskTime,TaskValue ())] | iTask k & iTask e & < k