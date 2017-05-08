module test_ifvalue_crash

import iTasks

Start :: *World -> *World
Start world = startEngine test2 world

teststore :: Shared String
teststore = sharedStore "test" "henk"

test2 :: Task ()
test2 = resethenk ||- updateSharedInformation "aha" [] teststore
	>>* [OnAction (Action "Exit") (always (return ()))]

resethenk :: Task String
resethenk = forever (watch teststore >>* [OnValue (ifValue (\s.s=="henk") (\s.set "" teststore))])
/*
test :: Task ()
test = watch teststore -|| updateSharedInformation "aha" [] teststore
 @! ()
	>^* [OnValue (ifValue (\s.s=="henk") ()]
	>>* [OnAction (Action "Exit") (always (return ()))]
	
	test = forever (watch teststore >>* [ -|| updateSharedInformation "aha" [] teststore
 @! ()
	>^* [OnValue (ifValue (\s.s=="henk") ()]
	>>* [OnAction (Action "Exit") (always (return ()))]*/