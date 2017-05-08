module test_ace_two_browser_instances

import iTasks
import iTasks.API.Extensions.Editors.Ace

Start :: *World -> *World
Start world = startEngine test world

simpleEr :: (AceOptions,AceState)
simpleEr = (
				{AceOptions|
					theme = "",
					mode = ""
				}
				,
				{AceState|
					lines = [],
					cursor = (0,0),
					selection = Nothing,
					disabled = False
				}
			)

teststore :: Shared (AceOptions,AceState)
teststore = sharedStore "test" simpleEr

test :: Task () 
test = (updateSharedInformation "test" [UpdateUsing id (\_ nsc -> nsc) aceEditor] teststore) @! ()