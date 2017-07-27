implementation module pageEditor

import iTasks
import pagetypes
import shares
import extraTaskCombinators
import qualified Data.Map as DM
//import callCpm
import directoryBrowsing
import createAndRunExec
import iTasks.API.Extensions.Editors.Ace
import iTasks.UI.Editor.Builtin
import iTasks._Framework.IWorld
import Text
import errorHandling

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
pageEditor ((actionOpen,pagenodeChooseFile),(actionAskImportPaths,pagenodeAskImportPaths),(actionNew,pagenodeCreateFile)) =
	(editors) 
	-&&-
	(repeatEveryTwoMinutes (build))
	 >>*	[   OnAction  actionOpen   			(always pagenodeChooseFile)
	 		,	OnAction actionAskImportPaths	(always (pagenodeAskImportPaths))
	 		,	OnAction ActionNew 				(always pagenodeCreateFile)
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

/*
showErrors :: ParallelTask ()
showErrors = \tasklist.viewSharedInformation "errors" [ViewUsing id (textArea 'DM'.newMap)] errorstate >>|- return ()
*/

//j is de lengte
subset :: Int Int [a] -> [a]
subset i j a = snd (splitAt i (fst (splitAt (j+i) a)))

before :: Int [a] -> [a]
before i a = fst (splitAt i a)

after :: Int [a] -> [a]
after i a = snd (splitAt i a)

//adds the interspersed element also as head
intersperse :: a [a] -> [a]
intersperse el [] = []
intersperse el l = foldr (\x y.[el,x: y]) [] l

viewSelection :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
viewSelection filename s = \tasklist. (viewSharedInformation "selected" [ViewUsing (\(ei,c). mrange2text (ei.EditorInfo.selection) ('DM'.get filename c)) (textArea 'DM'.newMap)] s >>|- return ())<<@ Title "View Selection"
where
	mrange2text _ Nothing = ""
	mrange2text Nothing (Just text) = "-"
	mrange2text (Just acerange) (Just text) = 
		selectText acerange text
		
selectText :: AceRange [String] -> String
selectText {start=(x1a,y1a),end=(x2a,y2a)} text
	#x1 = max 0 (min x1a ((length text) - 1))
	#x2 = max 0 (min x2a ((length text) - 1))
	#y1 = max 0 (min y1a ((textSize (text !! x1))))
	#y2 = max 0 (min y2a ((textSize (text !! x2))))
	= if (x1==x2) 
	(subString y1 (y2-y1) (text !! x1))
	((subString y1 (textSize (text !! x1)) (text !! x1)) +++ (concat (intersperse "\n" (subset (x1+1) (x2-x1-1) text))) +++ "\n" +++ (subString 0 y2 (text !! x2) )) 

vbtekst = ["Lorem Ipsum is slechts een proeftekst uit \n" ,
	"het drukkerij- en zetterijwezen. Lorem Ipsum is \n" ,
	"de standaard proeftekst in deze bedrijfstak sinds \n" ,
	"de 16e eeuw, toen een onbekende drukker een zethaak met \n" ,
	"letters nam en ze door elkaar husselde om een font-catalogus \n" ,
	"te maken. Het heeft niet alleen vijf eeuwen overleefd maar \n" ,
	"is ook, vrijwel onveranderd, overgenomen in elektronische letterzetting."]

/*de onderste zin is 74 lang*/  
vbselecties :: String
vbselecties = 
	(selectText {start=(-1,-1), end=(7,100)} vbtekst) +++ "\n\n\n" +++
	(selectText {start=(1,5), end=(1,5)} vbtekst) +++ "\n\n\n" +++
	(selectText {start=(1,5), end=(1,6)} vbtekst) +++ "\n\n\n" +++
	(selectText {start=(1,5), end=(2,9)} vbtekst)
      
helpwindows :: String (Shared (EditorInfo,Map String [String])) -> Task [(TaskTime,TaskValue ())]
helpwindows filename s = (parallel (embed
	[	(errorWindow filename s)
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
			lines = maybe ["could not get contents"] (\id.id) (fst ('DM'.getU filename contents)) ,
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

//autoWrite automatically writes at certain conditions. At the moment those are:
// - when the cursor is on a function line (a word followed by ::), the function name is put underneath it, if it isn't already.
autoWrite :: String (Shared (EditorInfo,Map String [String])) -> Task ()
autoWrite filename s = 
	watch s >>* [OnValue (ifValue (\(ei,c).case ('DM'.get filename c) of
		Nothing = False
		(Just content) = isAFunctionLine (content!!(fst ei.position)) if (((fst ei.position)+1)>=(length content)) "" (content!!((fst ei.position)+1)))
	(\(ei,c).check "1" >>| case ('DM'.get filename c) of
		Nothing = return ()
		(Just content) = check "2" >>|
			addFunctionName content filename (fst ei.position) s >>|- check "6" >>| autoWrite filename s)) ]

check :: String -> Task String
check s = viewInformation "" [] s

isAFunctionLine :: String String -> Bool
isAFunctionLine line nextline
	# splitted = split "::" line
	#fname = if (splitted==[]) "" (trim (splitted!!0))
	#alreadyThere = (indexOf fname (trim nextline))==0
	= (trim nextline <> "") && (length splitted==2) && (not alreadyThere)

isAFunctionLineTest :: [Bool]	
isAFunctionLineTest = [
	isAFunctionLine "" "",
	isAFunctionLine "::" "",
	isAFunctionLine "::" " ",
	isAFunctionLine " ::" "",
	isAFunctionLine "henk :: kaas saak" "",
	isAFunctionLine "henk :: kaas saak" " ",
	isAFunctionLine "henk :: kaas saak" "henk ",
	isAFunctionLine "henk :: kaas saak" "henkje ",
	isAFunctionLine "henk :: kaas saak" "appelsap"
	]
	
addFunctionName :: [String] String Int (Shared (EditorInfo,Map String [String])) -> Task ()
addFunctionName content filename findex s
	# fname = trim ((split "::" (content!!findex))!!0)
	# splitted = split " " (content!!(findex+1))
	# newline = if (length splitted>0) (fname+++" "+++(join " " (tl splitted))) (fname)
	= check "3" >>| upd (\c.'DM'.put filename (updateAt (findex+1) newline content) c) contents 
		>>|- check "4" >>| upd (\(ei,c). ({EditorInfo| ei & position = (findex,snd ei.position)},c)) s >>|- check "5" >>| return ()
	
	
		    
editor :: String (Shared ((Map String [String]),(Map String TaskId))) -> ParallelTask ()
editor filename cnt = \tasklist.(withShared editorInfo (\ei. 
	get cnt >>- \(c,utabs).
	get (taskListSelfId tasklist) >>- \selfid.
	set (c,'DM'.put filename selfid utabs) cnt >>|-
	(updateSharedInformation (name) [UpdateUsing id (\_ nsc -> nsc) aceEditor] (er ei filename))
	-&&-
	helpwindows filename (ei >*< contents) //-|| autoWrite filename (ei >*< contents) -|| viewSharedInformation "" [] ei 
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
		,
			OnAction (Action "placeText") (always (enterInformation "" [] >>= \s.placeText filename 2 s))	
		])<<@ Title name
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
	parallel [(Embedded,(waitf c tabs (updateTabs tabs))):(map (\x.(Embedded,(editor x (contents >*< tabs)))) ('DM'.keys c))] [] <<@ ApplyLayout (layoutSubs SelectRoot arrangeWithTabs))
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
updateTabs tabs = \tasklist. ut tabs tasklist 
	where
	ut :: (Shared (Map String TaskId)) -> ParallelTask ()
	ut tabs = \tasklist. watch (cnt tabs) 
		>>* [	OnValue		(ifValue (\(c,utabs). differentLengths c utabs) (\(c,utabs). (auxa utabs c tasklist) >>|- 
				watch tabs >>* [ OnValue (ifValue (\utabs. 'DM'.mapSize utabs == length ('DM'.keys c)) (\utabs. updateTabs tabs tasklist))]))]
	
	waitf :: (Map String [String]) (Shared (Map String TaskId)) (ParallelTask ()) -> ParallelTask ()
	waitf c tabs task = \tasklist.watch tabs >>* [ OnValue (ifValue (\utabs. 'DM'.mapSize utabs == length ('DM'.keys c)) (\utabs. task tasklist))] 
	
	auxa :: (Map String TaskId) (Map String [String]) (SharedTaskList ()) -> Task ()
	auxa utabs c tasklist = (aux_append utabs c tasklist) 
		>>|- (aux_remove utabs c tasklist)
	
	aux_append :: (Map String TaskId) (Map String [String]) (SharedTaskList ()) -> Task ()
	aux_append utabs c tasklist = mapTasksSequentially (\name.appendTask Embedded (editor name (cnt tabs)) tasklist >>|- return ()) ('DM'.keys ('DM'.difference c utabs))
	
	aux_remove :: (Map String TaskId) (Map String [String]) (SharedTaskList ()) -> Task ()
	aux_remove utabs c tasklist = mapTasksSequentially (\taskid. removeTask taskid tasklist) ('DM'.elems ('DM'.difference utabs c))
			    
