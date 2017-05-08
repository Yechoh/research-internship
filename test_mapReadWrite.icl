module test_mapReadWrite

import iTasks
import iTasks.API.Extensions.Editors.Ace

Start :: *World -> *World
Start world = startEngine test world

/*teststore :: Shared String
teststore = sharedStore "test" "henk"*/



tuplestore :: Shared (String,Bool)
tuplestore = sharedStore "tuple" ("",True)

tuple2TestRead :: (String,Bool) -> String
tuple2TestRead (s,b) = s

test2TupleWrite :: String (String,Bool) -> Maybe (String,Bool)
test2TupleWrite s1 (s2,b) = Just (s1,b)

test :: Task ()
test = (updateSharedInformation "test" [] mrw) -|| viewSharedInformation "tuple" [] tuplestore @! () where
	mrw = mapReadWrite (tuple2TestRead,test2TupleWrite) tuplestore