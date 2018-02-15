module test_symmetricLens

import iTasks
import qualified Data.Map as DM
import mapshare
import extraTaskCombinators


henkstore :: Shared (String)
henkstore = sharedStore "henkstore" "henk"

pietstore :: Shared (String)
pietstore = sharedStore "pietstore" "piet"

instance zero String
	where
		zero = ""

Start world = startEngine s world

s :: Task (String)
s = return (symmetricLens (\a b. a) (\b a. b) henkstore pietstore) >>- \(a,b).
    updateSharedInformation "" [] (a)
    -||
    updateSharedInformation "" [] (b)
    -||
    updateSharedInformation "" [] henkstore
    -||
    updateSharedInformation "" [] pietstore
