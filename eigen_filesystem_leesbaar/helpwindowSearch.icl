implementation module helpwindowSearch

import iTasks
import showCloogleResults
import extraTaskCombinators
import shares
import extraList
import content
import extraText

helpwindowSearch :: String -> Task ()
helpwindowSearch filename = withShared True (\slocal.
	((updateSharedInformation "" [] searchterm
	>^*
	[	(OnAction (Action "Search shared definitions") (hasValue \a. upd (\l.False) slocal >>|- return ()))
	,	(OnAction (Action "Search this file") (hasValue \a. upd (\l.True) slocal >>|- return ()))
	]) -|| (showResults slocal filename)) >>|- return ())<<@ ApplyLayout (arrangeHorizontal)

showResults :: (Shared Bool) String -> Task ()
showResults slocal filename = get slocal >>- \local. if local
		(get searchterm >>- \term. showFileSearchResults term filename ||- watch slocal)
		(get searchterm >>- \term. showCloogleResults term ||- watch slocal)
		>>*
			[ OnValue (ifValue (\newlocal. newlocal <> local) (\newlocal. showResults slocal filename))
			]

linesContaining :: String Int [String] -> [(Int,String)]
linesContaining needle i [] = []
linesContaining needle i [x:y]
| indexOf needle x <> -1 = [(i,x):linesContaining needle (i+1) y]
| otherwise = linesContaining needle (i+1) y

showFileSearchResults :: String String -> Task ()
showFileSearchResults term filename = contentOf filename >>- \content.
	(enterChoice "" [ChooseFromGrid id] (linesContaining term 1 content.lines)
	>>*
	[OnValue (hasValue \(i,line). showFileSearchResults term filename -&&- jumpToLine filename (i-1) )])
	>>|- return ()

cloogleFindDeprecated :: ParallelTask ()
cloogleFindDeprecated = \tasklist.
	first >>|- return ()
	where
	first =
		(enterInformation "" []
		>&^
		(\sma. ((viewInformation "" [] "" -||- viewInformation "" [] "")<<@ ApplyLayout(arrangeHorizontal)) ||- watch sma
			>>*
			[	(OnValue (ifValue (\ma. isJust ma) \(Just a).
				second2 sma a) )
			])) <<@ ApplyLayout (arrangeHorizontal)

second2 :: (ReadOnlyShared (Maybe String)) String -> Task ()
second2 sma a = get clooglestore >>- \db.
	(((enterChoice "" [ChooseFromGrid \result. (snd result).fe_representation] (findFunction a db))
	>&>
	\sma2. watch (sma >*< sma2))
		>>*
		[ (OnValue (ifValue (\(ma,ma2). isJust ma2) \(ma,Just a2).
		(second2 sma a -||- third a)<<@ ApplyLayout (arrangeHorizontal)))
		, (OnValue (ifValue (\(ma,ma2). maybe (False) (\newa. newa =!= a) ma) (\(Just newa,ma2).
		second2 sma newa )))
		])

third :: a -> Task ()
third a = (enterChoice "" [ChooseFromGrid id] ["To icl","To dcl"])
	>&>
	\sma2. watch sma2
	>>*
	[ (OnValue (ifValue (\ma2. maybe (False) (\a. a=="To icl") ma2) \(Just a2).  return ()))
	, (OnValue (ifValue (\ma2. maybe (False) (\a. a=="To dcl") ma2) \(Just a2).  return ()))
	]
