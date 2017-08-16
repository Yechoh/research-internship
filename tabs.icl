module tabs

import iTasks

Start :: *World -> *World
Start world = startEngine
	page
	world
	
page :: Task [(TaskTime,TaskValue ())]
page = list 
	where 
	list = parallel [(Embedded, phsw),(Embedded,phw)] [] //<<@ ApplyLayout (layoutSubs SelectRoot arrangeWithTabs)
	
phw :: (RWShared TaskListFilter (!TaskId,![TaskListItem ()]) [(!TaskId,!TaskAttributes)]) -> Task ()
phw a = 
	   (viewSharedInformation "safe" [] (taskListIds a)
		>>*[	OnAction (Action "add copy") (always (appendTask Embedded phw a ||- phw a))
			,	OnAction (Action "remove myself") (always ( get (taskListSelfId a) >>- \taskid. removeTask taskid a ||- phw a))
			]
	 >>- (\a.return ()))

pshw :: (RWShared TaskListFilter (!TaskId,![TaskListItem ()]) [(!TaskId,!TaskAttributes)]) -> Task ()
pshw a = 
	   (viewSharedInformation "weird" [] (taskListIds a)
		>>*[	OnAction (Action "add copy") (always (appendTask Embedded pshw a ||- pshw a))
			,	OnAction (Action "remove last task") (always ( get (taskListIds a) >>- \taskids. removeTask (hd (reverse taskids)) a ||- pshw a))
			]
	 >>- (\a.return ()))
	
