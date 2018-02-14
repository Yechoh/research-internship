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

map1 :: Map Int Int
map1 = 'DM'.fromList
		[
			(1,11),
			(2,22),
			(3,33)
		]

map2 :: Map Int String
map2 = 'DM'.fromList
		[
			(2,"2"),
			(3,"3"),
			(4,"4")
		]

//Start = ('DM'.difference map1 map2, 'DM'.difference map2 map1)

//Start = parallel

instance zero String
	where
		zero = ""

Start world = startEngine
	(
		f
	||-
		dynamicTabs
			(mapstore)
			(toString)
			(\k e.updateSharedInformation "" [] e >>* [OnAction (Action "Close") (always (return ()))])
	)
	world
