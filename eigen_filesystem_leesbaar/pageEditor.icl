implementation module pageEditor

import iTasks
import shares
import extraTaskCombinators
import qualified Data.Map as DM
import directoryBrowsing
import iTasks.Extensions.Editors.Ace
import errorHandling
import builddb
import CloogleDB
import dynamicTabs
import mapshare
import pageCreateFile
import pageAskImportPaths
import pageChooseFile
import helpwindowErrors
import helpwindowSearch
import helpwindowShare
import helpwindowSelected
import helpwindowImports
import helpwindowReplace
import iTasks.Internal.TaskEval
import content

pageEditor :: Task ()
pageEditor = accWorld(buildit) >>= \a. set a clooglestore >>|-
	(editors)
	-&&-
	(repeatEveryTwoMinutes (build))
	 >>*	[   OnAction  (Action "Open")   			(always pageChooseFile)
	 		,	OnAction (Action "Ask Import Paths")	(always (pageAskImportPaths))
	 		,	OnAction (Action "New")				(always pageCreateFile)
		    ]

embed :: [(Task a,String)] -> [(ParallelTaskType,ParallelTask a)]
embed l = map (\(t,n). (Embedded,(\tasklist. t <<@ (Title n)))) l

/*
focusNextReplacement :: [(Int,String)] (Int,Int) -> [(String,String)]
focusNextReplacement [] (line,character) = []
//focusNextReplacement [(i,s1)] (line,character) =
focusNextReplacement [(i,s1),(j,s2):z] (line,character)
| i == line || (i < line && j > line) = [(">"+++(toString i)+++"<",s1): map (\(i,s).(" "+++(toString i)+++" ",s)) [(j,s2):z]]
| otherwise = [(" "+++toString(i)+++" ",s1): focusNextReplacement [(j,s2):z] (line,character)]

showReplace :: String String -> Task ()
showReplace filename term = contentOf filename >>- \content.
	(enterChoice "" [ChooseFromGrid id] (focusNextReplacement (linesContaining term 1 content.lines) content.cursor)) >>|- return ()
*/

helpwindows :: String -> Task [(TaskTime,TaskValue ())]
helpwindows filename = (parallel (embed
	[	(helpwindowErrors, "Errors")
	,	(helpwindowSelected filename, "Selected")
	,	(helpwindowSearch filename, "Search")
	,	(helpwindowReplace filename "", "Replace")
	, 	(helpwindowShare filename, "Share")
	//, 	(helpwindowImports filename, "Imports")
	]) []) <<@ ApplyLayout (arrangeWithTabs)

//autoWrite automatically writes at certain conditions. At the moment those are:
// - when the cursor is on a function line (a word followed by ::), the function name is put underneath it, if it isn't already.
/*autoWrite :: (Shared (String,String)) -> Task ()
autoWrite sLinesAtCursor =
	watch sLinesAtCursor >>* [OnValue (ifValue (\(lineAt,lineAfter). isAFunctionLine lineAt lineAfter)
	(\(lineAt,lineAfter).
			addFunctionName lineAt lineAfter sLinesAtCursor >>|- autoWrite sLinesAtCursor)) ]

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

addFunctionName :: String String (Shared (String,String)) -> Task ()
addFunctionName lineAt lineAfter sLinesAtCursor
	# fname = trim ((split "::" lineAt)!!0)
	# splitted = split " " (lineAfter)
	# newline = if (length splitted>0) (fname+++" "+++(join " " (tl splitted))) (fname)
	= upd (\(a,b). (a,newline)) sLinesAtCursor >>|- return ()

/*getLinesAtcursor :: AceState -> (String,String)
getLinesAtcursor content =*/
*/
editor :: String (Shared AceState) -> Task ()
editor filename content =
	(((updateSharedInformation "" [UpdateUsing (\c. (zero,c)) (\c (a,b) -> b) aceEditor] content) -||
	helpwindows filename //-|| autoWrite (mapReadWrite (getLinesAtcursor,putLinesAtCursor) content)
	)
	>>*[	OnAction ActionSaveAs		(hasValue
			(\c. saveFileAs filename (foldr joinWithNewline "" c.lines) >>|- editor filename content))
	 	,
	 		OnAction ActionClose		(hasValue
			(\c. saveFile filename (foldr joinWithNewline "" c.lines) >>|-
			return ()))
		,
			OnAction (Action "Restart") (always
			(editor filename content))
		]) >>|- return ()
/*
simpleEditor :: String -> ParallelTask ()
simpleEditor filename = \tasklist.
	*/

editors :: Task [(Int,TaskValue ())]
editors = dynamicTabs contents (dropDirectory) (editor)
