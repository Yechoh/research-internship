module test_mapshare

import iTasks
import qualified Data.Map as DM
import mapshare
import extraTaskCombinators

mapstore :: Shared (Map Int String)
mapstore = sharedStore "mapstore" ('DM'.fromList
	[
		(1,"001"),
		(2,"010"),
		(3,"011"),
		(4,"100")
	])

stringstore :: Shared (String)
stringstore = sharedStore "stringstore" "henk"

addf :: Task (Map Int String)
addf = (enterInformation "" [] >>= \(key,el). upd (\map. 'DM'.put key el map) mapstore)

delf :: Task (Map Int String)
delf = (enterInformation "" [] >>= \key. upd (\map. 'DM'.del key map) mapstore)

f :: Task ()
f = (viewSharedInformation "" [] mapstore
		>>*
		[	OnAction (Action "Add") (always (f -|| addf))
		,	OnAction (Action "Delete") (always (f -|| delf))
		])

instance zero String
	where
		zero = ""

Start world = startEngine s2 world

s2 :: Task ()
s2 =
    (
		updateSharedInformation "" [] mapstore
	-||
		updateSharedInformation "" [] (mapMaybeLens "hans" mapstore))

s :: Task ()
s =
	((
		updateSharedInformation "" [] mapstore
	-||
		updateSharedInformation "" [] stringstore) >>* [OnAction (Action "msputs") (always(return () >>|- s))]
	)
