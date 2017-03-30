module tabs

import iTasks

Start :: *World -> *World
Start world = startEngine
	//(allTasks [helloWorld <<@ Title "a", enterString <<@ Title "b", enterString <<@ Title "c"] <<@ ApplyLayout (layoutSubs SelectRoot arrangeWithTabs))
	page
	world

page :: Task ()
page = withShared "" (\text. tabs text -&&- UpdateSharedInformation "" [] text) >>= return ()

tabs :: (Shared String) -> Task ()
tabs text = 
	

the_workflow :: [Workflow]
the_workflow = 
	[workflow ("Hello world") 			 	"View a constant string" 			helloWorld
	,workflow ("Enter a string") 		 	"Entering a string" 				enterString
	,workflow ("Enter an integer") 		 	"Entering an integer" 				enterInt
	]
	
helloWorld :: Task String
helloWorld = viewInformation "You have a message from iTasks:" [] "Hello world!" 

enterString :: Task String
enterString = enterInformation "Enter a string" []

enterInt :: Task Int
enterInt = enterInformation "Enter an integer" []