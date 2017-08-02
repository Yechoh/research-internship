module test_dynamicTabs

import iTasks
import dynamicTabs
import qualified Data.Map as DM

mapstore :: Shared (Map Int String)
mapstore = sharedStore "mapstore" ('DM'.fromList 
	[
		(1,"001"),
		(2,"010"),
		(3,"011"),
		(4,"100")
	])

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

Start world = startEngine
	(
		f
	||-
		dynamicTabs 
			(mapstore) 
			(toString) 
			(\k e.updateSharedInformation "" [] e >>* [OnAction (Action "Close") (always (return ()))])
			("harige harry")
	)
	world