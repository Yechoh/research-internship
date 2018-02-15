implementation module helpwindowReplace

import iTasks
import content
import shares
import extraText
import extraTaskCombinators

replaceNext :: [String] String String (Int,Int) -> [String]
replaceNext text search replace (line,character)
	# (lbefore,lafter) = splitAt line text
	# (lcurrent,lafter) = (hd lafter,tl lafter)
	# indexCurrent = indexOfAfter character search lcurrent
	= if (indexCurrent <> -1)
	(lbefore ++ [(replaceSubString search replace lcurrent):lafter])
	(lbefore ++ [lcurrent: replaceNext` search replace lafter])
	where
		replaceNext` :: String String [String] -> [String]
		replaceNext` search replace [] = []
		replaceNext` search replace [x:y] =
		 	if (indexOf search x <> -1)
			([replaceSubString search replace x:y])
			([x:replaceNext` search replace y])

:: Q = Before | After

replaceInfo :: String [String] (Int,Int) (Int,Int) Q -> [((Int,Int),String,Bool)]
replaceInfo needle [] (line,character) (i,j) q = []
replaceInfo needle [s:r] (line,character) (i,j) Before
| indexOfAfter j needle s <> -1
	| (i== line && indexOfAfter j needle s >= character) || i>line = [((i,(indexOfAfter j needle s)),s,True):replaceInfo needle [s:r] (line,character) (i,(indexOfAfter j needle s)+1) After]
	| otherwise = [((i,(indexOfAfter j needle s)),s,False):replaceInfo needle [s:r] (line,character) (i,(indexOfAfter j needle s)+1) Before]
| otherwise = replaceInfo needle r (line,character) (i+1,0) Before
replaceInfo needle [s:r] (line,character) (i,j) After
| indexOfAfter j needle s <> -1 = [((i,(indexOfAfter j needle s)),s,False):replaceInfo needle [s:r] (line,character) (i,(indexOfAfter j needle s)+1) After]
| otherwise = replaceInfo needle r (line,character) (i+1,0) After

replaceview :: ((Int,Int),String,Bool) -> (String,String)
replaceview ((i,j),s,True) = (concat [">(",toString (i+1),",",toString j,")<"],s)
replaceview ((i,j),s,False) = (concat [" (",toString (i+1),",",toString j,") "],s)

replace :: ((Int,Int),String,Bool) String String [String] String -> Task ()
replace ((line,character),s,b) term replaceterm text filename
# (lbefore,lafter) = splitAt line text
# (lcurrent,lafter) = (hd lafter,tl lafter)
= updateContent filename (\content. {content & lines = (lbefore ++ [replaceFirstSubStringAt character term replaceterm lcurrent: lafter])}) >>|- return ()

viewreplace :: String String String -> Task [((Int,Int),String,Bool)]
viewreplace filename term replaceterm = contentOf filename >>- \content.
	enterChoice "Click to replace" [ChooseFromGrid replaceview] (replaceInfo term content.lines content.cursor (0,0) Before)
	>>*
	[OnValue (hasValue \info. contentOf filename >>- \content. replace info term replaceterm content.lines filename >>|- (viewreplace filename term replaceterm))]


helpwindowReplace :: String String -> Task ()
helpwindowReplace filename replaceterm = get searchterm >>- \term. contentOf filename >>- \content.
	((updateSharedInformation "Search" [] searchterm -&&- updateInformation "Replace" [] replaceterm) -|| viewreplace filename term replaceterm)<<@ ApplyLayout (arrangeHorizontal)
	>>*
	[	(OnAction (Action "Update view") (ifValue (\((search,replace)). search <> "") (\((search,replace)). helpwindowReplace filename replace)))
	,	(OnAction (Action "Replace all") (ifValue (\((search,replace)). search <> "") (\((search,replace)). updateContent filename (\content. {content & lines=(map (\line.replaceSubString search replace line) content.lines)}) ||- helpwindowReplace filename replace )))
	]
