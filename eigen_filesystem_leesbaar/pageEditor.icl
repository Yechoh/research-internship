implementation module pageEditor

import iTasks
import pagetypes
import shares
import extraTaskCombinators
import qualified Data.Map as DM
import callCpm
import directoryBrowsing
import createAndRunExec
import iTasks.API.Extensions.Editors.Ace
import iTasks.UI.Editor.Builtin
import iTasks._Framework.IWorld
import Text

:: Shortcut = No_shortcut 
			| Ctrl_slash 
			| Ctrl_backslash 
			| Ctrl_Shift_backslash 
			| Ctrl_equals 
			| Ctrl_Shift_equals 
			| Ctrl_b //
			| Ctrl_d
			| Ctrl_Shift_d
			| Ctrl_e
			| Ctrl_Shift_e
			| Ctrl_i
			| Ctrl_l
			| Ctrl_m
			| Ctrl_n
			| Ctrl_o
			| Ctrl_r
			| Ctrl_Shift_r
			| Ctrl_s
			| Ctrl_Shift_s
			| Ctrl_Alt_s
			| Ctrl_Shift_Alt_s
			| Ctrl_w
			| Ctrl_Shift_w

:: EditorInfo = 
	{
		shortcuts :: [Shortcut],
		selection :: Maybe AceRange,
		position :: (Int,Int),
		theme :: String,
		readOnly :: Bool,
		prev_time :: Time
	}
	
derive class iTask EditorInfo, Shortcut

editorInfo = 
	{
		shortcuts = [],
		position = (0,0),
		selection = Nothing,
		theme = "",
		readOnly = False,
		prev_time = {Time|hour=0,min=0,sec=0}
	}
/*
editorRecord = 
	{
		content = [""],
		prev_time = iworldLocalTime,
		shortcuts = [],
		position = (0,0),
		selection_position = (0,0),
		theme = "",
		readOnly = False
	}
*/
pageEditor :: EditorRedirects -> Task ()
pageEditor ((actionOpen,pagenodeChooseFile),(actionAskImportPaths,pagenodeAskImportPaths)) =
	(editors) 
	// -&&-
	// helpwindows) 
	 //-&&-
	 //(repeatEverySecond (cpmSetErrorstate))
	 >>*	[   OnAction  actionOpen   	(always pagenodeChooseFile)
	 		,	OnAction actionAskImportPaths	(always (pagenodeAskImportPaths))
		    ]
/*
latest :: Time Time -> Time
latest t1 t2
 | t1.hour > t2.hour = t1
 | t1.hour < t2.hour = t2
 | t1.min > t2.min = t1
 | t1.min < t2.min = t2
 | t1.sec > t2.sec = t1
 | otherwise = t2
*/

embed :: [ParallelTask a] -> [(ParallelTaskType,ParallelTask a)]
embed l = map (\t. (Embedded,t)) l

showErrors :: ParallelTask ()
showErrors = \tasklist.viewSharedInformation "errors" [ViewUsing id (textArea 'DM'.newMap)] errorstate >>|- return ()

//j is de lengte
subset :: Int Int [a] -> [a]
subset i j a = snd (splitAt i (fst (splitAt (j+i) a)))

before :: Int [a] -> [a]
before i a = fst (splitAt i a)

after :: Int [a] -> [a]
after i a = snd (splitAt i a)

intersperse :: a [a] -> [a]
intersperse el [] = []
intersperse el l = foldr (\x y.[el,x: y]) [] l

viewSelection :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
viewSelection filename s = \tasklist. viewSharedInformation "selected" [ViewUsing (\(ei,c). mrange2text (ei.EditorInfo.selection) ('DM'.get filename c)) (textArea 'DM'.newMap)] s >>|- return ()
where
	mrange2text Nothing (Just text) = "-"
	mrange2text (Just {start=(x1,y1),end=(x2,y2)}) (Just text) = 
		if (x1==x2) 
		(subString y1 (y2-y1) (text !! x1))
		((subString y1 (textSize (text !! x1)) (text !! x1)) +++ (concat (intersperse "\n" (subset (x1+1) (x2-x1-1) text))) +++ "\n" +++ (subString 0 y2 (text !! x2) )) 
		


helpwindows :: String (Shared (EditorInfo,Map String [String])) -> Task [(TaskTime,TaskValue ())]
helpwindows filename s = (parallel (embed
	[	(showErrors)
	,	viewSelection filename s	
	]) [])<<@ ApplyLayout (layoutSubs SelectRoot arrangeWithTabs)




eiAndContents2ErRead :: String (EditorInfo,Map String [String])  -> (!AceOptions,!AceState)
eiAndContents2ErRead filename (ei,contents)  =
	(
		{AceOptions|
			theme = ei.EditorInfo.theme,
			mode = "mode-haskell"//"C:\\Users\\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\research-internship\\ace\\ace-master\\lib\\ace\\mode\\clean.js"
		}
		,
		{AceState|
			lines = fromJust (fst ('DM'.getU filename contents)),
			cursor = ei.EditorInfo.position,
			selection = ei.EditorInfo.selection,
			disabled = ei.EditorInfo.readOnly
		}
	)

er2EiAndContentsWrite :: String (!AceOptions,!AceState) (EditorInfo,Map String [String]) -> Maybe (EditorInfo,(Map String [String]))
er2EiAndContentsWrite filename (ao,as) (ei,contents) =
	Just (
		{EditorInfo|
			shortcuts = ei.EditorInfo.shortcuts,
			selection = as.AceState.selection,
			position = as.AceState.cursor,
			theme = ao.AceOptions.theme,
			readOnly=as.AceState.disabled,
			prev_time=ei.EditorInfo.prev_time
		}, 
		'DM'.put filename as.lines contents
	)
		    
editor :: String (Shared ((Map String [String]),(Map String TaskId))) -> ParallelTask ()
editor filename cnt = \tasklist.withShared editorInfo (\ei. 
	get cnt >>- \(c,utabs).
	get (taskListSelfId tasklist) >>- \selfid.
	set (c,'DM'.put filename selfid utabs) cnt >>|-
	(updateSharedInformation (name) [UpdateUsing id (\_ nsc -> nsc) aceEditor] (er ei filename))
	-&&-
	helpwindows filename (ei >*< contents)
	)
	>^*[	OnAction ActionSaveAs		(always 
			(contentOf filename >>- \content. 
			saveFileAs filename content ))
	 	]
	>>*[	OnAction ActionClose		(always (
			contentOf filename >>- \content. 
			saveFile filename content >>|- 
			get cnt >>- \(ucontents,utabs).
			set (('DM'.del filename ucontents),('DM'.del filename utabs)) cnt >>|-
			return ()))
		]
	where
	er ei filename = mapReadWrite ((eiAndContents2ErRead filename), (er2EiAndContentsWrite filename)) (ei >*< contents)
	name = dropDirectory filename
/*
simpleEditor :: String -> ParallelTask ()
simpleEditor filename = \tasklist. 
	*/
editors :: Task [(TaskTime,TaskValue ())]
editors = 
	get contents >>- \c. withShared 'DM'.newMap (\tabs.
	parallel /*[(Embedded,(waitf c tabs (updateTabs tabs))):*/(map (\x.(Embedded,(editor x (contents >*< tabs)))) ('DM'.keys c))/*]*/ [] <<@ ApplyLayout (layoutSubs SelectRoot arrangeWithTabs))
	where
	waitf :: (Map String [String]) (Shared (Map String TaskId)) (ParallelTask ()) -> ParallelTask ()
	waitf c tabs task = \tasklist.watch tabs >>* [ OnValue (ifValue (\utabs. 'DM'.mapSize utabs == length ('DM'.keys c)) (\utabs. task tasklist))] 
	
containsElsNotIn :: [a] [a] -> Bool | Eq a
containsElsNotIn a b = not(removeMembers a b == [])

mapTasksSequentially :: (b -> Task ()) [b] -> Task ()
mapTasksSequentially f l = foldr (\e t. t >>|- f e) (return ()) l

cnt :: (Shared (Map String TaskId)) -> Shared ((Map String [String]),(Map String TaskId))
cnt tabs = (contents >*< tabs)

differentLengths :: (Map String [String]) (Map String TaskId) -> Bool
differentLengths c utabs =  not ('DM'.mapSize c == 'DM'.mapSize utabs)

updateTabs :: (Shared (Map String TaskId)) -> ParallelTask ()
updateTabs tabs = \tasklist. watch (cnt tabs)
	>^* [	OnValue		(ifValue (\(c,utabs). differentLengths c utabs) (\(c,utabs). (auxa utabs c tasklist)))]
	>>|- return ()
	where
	auxa :: (Map String TaskId) (Map String [String]) (SharedTaskList ()) -> Task ()
	auxa utabs c tasklist = (aux_append utabs c tasklist) 
		>>|- (aux_remove utabs c tasklist)
	
	aux_append :: (Map String TaskId) (Map String [String]) (SharedTaskList ()) -> Task ()
	aux_append utabs c tasklist = mapTasksSequentially (\name.appendTask Embedded (editor name (cnt tabs)) tasklist >>|- return ()) ('DM'.keys ('DM'.difference c utabs))
	
	aux_remove :: (Map String TaskId) (Map String [String]) (SharedTaskList ()) -> Task ()
	aux_remove utabs c tasklist = mapTasksSequentially (\taskid. removeTask taskid tasklist) ('DM'.elems ('DM'.difference utabs c))
			    
