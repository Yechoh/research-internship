implementation module pageEditor

import iTasks
import pagetypes
import shares
import extraTaskCombinators
import qualified Data.Map as DM
//import callCpm
import directoryBrowsing
//import createAndRunExec
import iTasks.Extensions.Editors.Ace
import iTasks.UI.Editor.Builtin, iTasks.UI.Layout
import extraText
import errorHandling
import iTasks.Extensions.DateTime
import iTasks.Internal.TaskEval
import builddb
import CloogleDB
import doubleEnterChoice
import extraList

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
pageEditor ((actionOpen,pagenodeChooseFile),(actionAskImportPaths,pagenodeAskImportPaths),(actionNew,pagenodeCreateFile)) = accWorld(buildit) >>= \a. set a clooglestore >>| //return ()
	//viewInformation "" [] a
	//||-
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

linesContaining :: String Int [String] -> [(Int,String)]
linesContaining needle i [] = []
linesContaining needle i [x:y]
| indexOf needle x <> -1 = [(i,x):linesContaining needle (i+1) y]
| otherwise = linesContaining needle (i+1) y

replaceNext :: [String] String String EditorInfo -> [String]
replaceNext text search replace ei
	# line = fst ei.position
	# character = snd ei.position
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
= setContent filename (lbefore ++ [replaceFirstSubStringAt character term replaceterm lcurrent: lafter]) >>|- return ()

viewreplace :: String [String] EditorInfo String String -> Task [((Int,Int),String,Bool)]
viewreplace term text ei filename replaceterm =
	enterChoice "Click to replace" [ChooseFromGrid replaceview] (replaceInfo term text ei.position (0,0) Before)
	>>*
	[OnValue (hasValue \info. contentLinesOf filename >>- \text. replace info term replaceterm text filename >>|- (viewreplace term text ei filename replaceterm))]


replaceWindow :: String String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
replaceWindow filename replaceterm store = \tasklist. (get searchterm >>- \term. contentLinesOf filename >>- \text. get store >>- \(ei,c).
	((updateSharedInformation "Search" [] searchterm -&&- updateInformation "Replace" [] replaceterm) -|| viewreplace term text ei filename replaceterm)<<@ ApplyLayout (arrangeHorizontal)
	>>*
	[	(OnAction (Action "Update view") (ifValue (\((search,replace)). search <> "") (\((search,replace)). replaceWindow filename replace store tasklist)))
	,	(OnAction (Action "Replace all") (ifValue (\((search,replace)). search <> "") (\((search,replace)). contentLinesOf filename >>- \text. (setContent filename (map (\line.replaceSubString search replace line) text) ||- replaceWindow filename replace store tasklist))))
	]) <<@ (Title "Replace")

/*
replaceWindow :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
replaceWindow filename store = \tasklist.
	((updateSharedInformation "Search" [] searchterm -&&- enterInformation "Replace" [])
	>>*
	[	(OnAction (Action "Show") (ifValue (\(search,replace). search <> "") (\(search,replace).contentLinesOf filename >>- \text. showReplace search text store)))
	,	(OnAction (Action "Replace next") (ifValue (\(search,replace). search <> "") (\(search,replace).get store >>- \(ei,c). contentLinesOf filename >>- \text. setContent filename (replaceNext text search replace ei) ||- replaceWindow filename store tasklist)))
	,	(OnAction (Action "Replace all") (ifValue (\(search,replace). search <> "") (\(search,replace).contentLinesOf filename >>- \text. (setContent filename (map (\line.replaceSubString search replace line) text) ||- replaceWindow filename store tasklist))))]
	)  >>| return ()
*/

cloogleFind :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
cloogleFind filename store = \tasklist. (cloogleFind` filename store) <<@ (Title "Search") >>|- return ()

cloogleFind` :: String (Shared (EditorInfo,Map String [String])) -> Task String
cloogleFind` filename store = withShared True (\slocal.
	((updateSharedInformation "" [] searchterm
	>^*
	[	(OnAction (Action "Search shared definitions") (hasValue \a. upd (\l.False) slocal >>|- return ()))
	,	(OnAction (Action "Search this file") (hasValue \a. upd (\l.True) slocal >>|- return ()))
	]) -|| (showResults slocal filename store)))<<@ ApplyLayout (arrangeHorizontal)


focusNextReplacement :: [(Int,String)] (Int,Int) -> [(String,String)]
focusNextReplacement [] (line,character) = []
//focusNextReplacement [(i,s1)] (line,character) =
focusNextReplacement [(i,s1),(j,s2):z] (line,character)
| i == line || (i < line && j > line) = [(">"+++(toString i)+++"<",s1): map (\(i,s).(" "+++(toString i)+++" ",s)) [(j,s2):z]]
| otherwise = [(" "+++toString(i)+++" ",s1): focusNextReplacement [(j,s2):z] (line,character)]

showReplace :: String [String] (Shared (EditorInfo,Map String [String])) -> Task ()
showReplace term text store = get store >>- \(ei,c).
	(enterChoice "" [ChooseFromGrid id] (focusNextReplacement (linesContaining term 1 text) ei.position)) >>|- return ()

showFileSearchResults :: String String (Shared (EditorInfo,Map String [String])) -> Task ()
showFileSearchResults term filename store = contentLinesOf filename >>- \text.
	(enterChoice "" [ChooseFromGrid id] (linesContaining term 1 text)
	>>*
	[OnValue (hasValue \(i,line). showFileSearchResults term filename store -&&- jumpToLine store (i-1) )])
	>>|- return ()

showResults :: (Shared Bool) String (Shared (EditorInfo,Map String [String])) -> Task ()
showResults slocal filename store = get slocal >>- \local. if local
		(get searchterm >>- \term. showFileSearchResults term filename store ||- watch slocal)
		(get searchterm >>- \term. showCloogleResults term ||- watch slocal)
		>>*
			[ OnValue (ifValue (\newlocal. newlocal <> local) (\newlocal. showResults slocal filename store))
			]

showCloogleResults :: String -> Task ()
showCloogleResults a = get clooglestore >>- \db.
	((enterChoice "" [ChooseFromGrid \result. (snd result).fe_representation] (findFunction a db))
	>&^
	\a.return ())
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
		/*chooseTaskBasedOnFeed2
			(enterInformation "" [])
			(\searchterm."")
			(\searchterm.get clooglestore >>- \db. return (findFunction searchterm db))
			(\result. maybe "" id ((snd result).fe_representation))
			(\searchterm result. viewInformation "" [] result >>| return ())/*chooseTaskBasedOnFeed
				(return result)
				(\result. maybe "" id ((snd result).fe_representation))
				(\result.["To icl","To dcl"])
				(\result action. viewInformation "action" [] action >>| return ())
			)*/*/

/*
cloogleFind :: ParallelTask ()
cloogleFind = \tasklist. cloogleFind`
	where
	cloogleFind` =
		enterInformation "" []
			>>*
			[OnAction (Action "CloogleFind") (hasValue (\a.
				cloogleFind`
				-||
				(
					get clooglestore >>-
					\db. (viewInformation "" [] (findType a db)) >>| return ())))]
*/

viewSelection :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
viewSelection filename s = \tasklist. viewSelection` filename s <<@ (Title "Selected")
	where
	viewSelection` filename s =
		viewSharedInformation "selected" [ViewUsing (\(ei,c). mrange2text (ei.EditorInfo.selection) ('DM'.get filename c)) (textArea 'DM'.newMap)] s
			>>*
			[OnAction (Action "Search shared definitions") (hasValue (\(ei,c).
				((viewSelection` filename s
				-||
				(
					showCloogleResults (mrange2text (ei.EditorInfo.selection) ('DM'.get filename c)) >>|- return ()))<<@ ApplyLayout(arrangeHorizontal))))]

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

shareWindow :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
shareWindow filename s = \tasklist. ((get dclStore >>- \dcls. maybe (show []) (show) ('DM'.get filename dcls) >>* [OnAction (Action "Refresh") (always (shareWindow filename s tasklist))]) >>|- return ()) <<@ (Title "Shared")
where
	show dcl
	# imports = filter (\(x,y,z). startsWith "import " y || startsWith "derive " y) dcl
	# functions = filter (\(x,y,z). not (startsWith "import " y || startsWith "derive " y)) dcl
	= functiontasks (importtasks (headline) imports) functions
	where
		importtasks starttask imports = (foldr (\d t. t ||- showSharableImport filename d) (starttask) imports)
		functiontasks starttask functions = (foldr (\d t. t ||- showSharableFunction filename d) (starttask) functions)
		headline = ((((viewInformation "" [] "Shared:" -&&- viewInformation "" [] "Definition:")<<@ ApplyLayout (arrangeHorizontal)) -&&- viewInformation "" [] "Description:")<<@ ApplyLayout (arrangeHorizontal)) >>|- return ()

swapshared :: Sharenum -> Sharenum
swapshared Sharedi = Unsharedi
swapshared Unsharedi = Sharedi
swapshared Deprecatedf = Deprecatedf //should never occur
swapshared Sharedf = Unsharedf
swapshared Unsharedf = Sharedf

/*
showSharable :: [(Sharenum,String,Comment)]
showSharable filename dcl =
 	enterChoice "Shared" [ChooseFromGrid (\dcl. fst3 dcl === Sharedi || fst3 dcl === Sharedf)] dcl >>* [OnValue (hasValue (\(x,y,z). upd (\dcls. 'DM'.put filename (\dcl. map (\(x2,y2,z2). if (y2 == y) (swapshared x,y2,z2) (x2,y2,z2) ) dcl) dcls) dclsStore))]
	-&&-
	enterChoice "Definition" [ChooseFromGrid (snd3)] dcl
	-&&-
	enterChoice "Description"
*/

showSharableImport :: String (Sharenum,String,Comment) -> Task ()
showSharableImport filename (sharenum,def,desc) =
 	(((viewInformation "" [] (sharenum === Sharedi)
	-&&-
	viewInformation "" [] def)<<@ ApplyLayout (arrangeHorizontal))
	-&&-
	viewInformation "" [] "")<<@ ApplyLayout (arrangeHorizontal)
	>>*
	[ OnAction (Action (if (sharenum === Sharedi) "Unshare" "Share"))  (hasValue (\((x,y),z). upd (\dcls.('DM'.alter (\(Just dcl). Just (swapmap y dcl)) filename dcls)) dclStore ||- showSharableImport filename (swapshared sharenum,y,z)))
	]
	where
		swapmap :: String -> ([(Sharenum,String,Comment)] -> [(Sharenum,String,Comment)])
		swapmap y = (\dcl. map (\(x2,y2,z2). if (y2 == y) (swapshared sharenum,y2,z2) (x2,y2,z2) ) dcl)

showSharableFunction :: String (Sharenum,String,Comment) -> Task ()
showSharableFunction filename (sharenum,def,desc) =
 	(((viewInformation "" [] (sharenum === Sharedf)
	-&&-
	viewInformation "" [] def)<<@ ApplyLayout (arrangeHorizontal))
	-&&-
	updateInformation "" [UpdateUsing id (\a b. b) (textArea 'DM'.newMap)] desc)<<@ ApplyLayout (arrangeHorizontal)
	>>*
	[ OnAction (Action (if (sharenum === Sharedf) "Unshare" "Share"))  (hasValue (\((x,y),z). upd (\dcls.('DM'.alter (\(Just dcl). Just (swapmap y dcl)) filename dcls)) dclStore ||- showSharableFunction filename (swapshared sharenum,y,z)))
	, OnAction (Action "Save description") (hasValue \((x,y),z). upd (\dcls. ('DM'.alter (\(Just dcl). Just (commentmap y z dcl)) filename dcls)) dclStore ||- showSharableFunction filename (sharenum,y,z))
	]
	where
		swapmap :: String -> ([(Sharenum,String,Comment)] -> [(Sharenum,String,Comment)])
		swapmap y = (\dcl. map (\(x2,y2,z2). if (y2 == y) (swapshared sharenum,y2,z2) (x2,y2,z2) ) dcl)

		commentmap :: String Comment -> ([(Sharenum,String,Comment)] -> [(Sharenum,String,Comment)])
		commentmap y z = (\dcl. map (\(x2,y2,z2). if (y2 == y) (x2,y2,z) (x2,y2,z2) ) dcl)


helpwindows :: String (Shared (EditorInfo,Map String [String])) -> Task [(TaskTime,TaskValue ())]
helpwindows filename s = (parallel (embed
	[	(errorWindow filename s)
	,	(viewSelection filename s)
	,	(cloogleFind filename s)
	,	(replaceWindow filename "" s)
	, 	(shareWindow filename s)
	]) []) <<@ ApplyLayout (arrangeWithTabs)




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
	>>*[	OnAction ActionSaveAs		(always
			((contentOf filename) >>- \content.
			(saveFileAs filename content)<<@ Title name >>|- editor filename cnt tasklist))
	 	,
	 		OnAction ActionClose		(always (
			contentOf filename >>- \content.
			saveFile filename content >>|-
			get cnt >>- \(ucontents,utabs).
			set (('DM'.del filename ucontents),('DM'.del filename utabs)) cnt >>|-
			return ()))
		,
			OnAction (Action "Restart") (always
			(editor filename cnt tasklist))
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
	parallel [(Embedded,(waitf c tabs (updateTabs tabs))):(map (\x.(Embedded,(editor x (contents >*< tabs)))) ('DM'.keys c))] [] <<@ ApplyLayout (arrangeWithTabs))
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
