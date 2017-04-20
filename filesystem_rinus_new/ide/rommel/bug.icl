module bug

import iTasks


Start world = startEngine bug world

mystore = sharedStore  "store" 99

bug = first "oeps">>| second

second = viewInformation "second" [] ""

first name
	=	updateInformation "type in a value?" [] name
	>>*		[	OnAction ActionOk		(hasValue (\name -> return name
													>>| set 1234  mystore
													>>| return name))
			,	OnAction ActionCancel	(always (return name))
			]
//	>>= return 		// this is needed while I would expect it is not !!!

/*
				saveFileAs saname content 
					= 		updateInformation "Where to write to ?" [] saname

					>>*		[	OnAction ActionSave		(hasValue (	\name -> 	addToRecentFiles (False,name)
																	>>|			writeToFile name (convert_cr content) 
																	>>| 		return name))
						 	,	OnAction ActionCancel	(always (return saname))
						 	]
					>>= return 		// this is needed while I would expect it is not !!!
	*/