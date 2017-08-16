implementation module errorHandling

import iTasks
import shares
import System.File
import directoryBrowsing
import _SystemArray
import extraTaskCombinators
import qualified Data.Map as DM
import iTasks.Extensions.Process
import extraText
import doubleEnterChoice
import builddb
import CloogleDB
import System.OS

//!String !(Shared (!AceOptions,!AceState))

errorWindow :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
errorWindow nameOfFileInEditor editorStore
	= \tasklist.
	(chooseTaskBasedOnElementOfSharedList
		""
		errorStore
		(\a.diagnosedErrorToDescription (diagnose a))
		(\a.diagnosedErrorToSolutions (diagnose a))
		(\a b.diagnosedErrorToTask (diagnose a) b editorStore)
	 )<<@ (Title "Errors")
	//(
	/*(doubleEnterChoiceWithShared
		""
		[ChooseFromGrid id]
		errorStore
		(\a.diagnosedErrorToDescription(diagnose(a)))
		(\a.[ChooseFromGrid fst])
		(\a.diagnosedErrorToSolutions (diagnose a) editorStore)
	)>>- \b.snd b) <<@ (Title "Errors")*/

:: DiagnosedError :== (FileName, Int, ErrorDiagnosis)
:: ErrorDiagnosis = UndefinedVar String | Unknown String

diagnosedErrorToDescription :: DiagnosedError -> String
diagnosedErrorToDescription (_,_,UndefinedVar varname) = varname+++" undefined"
diagnosedErrorToDescription (_,_,Unknown err) = "unknown error "+++err

diagnosedErrorToSolutions :: DiagnosedError -> [String]
diagnosedErrorToSolutions (filename,i,diagnosis)= ["Go to error"] ++
	case diagnosis of
	UndefinedVar varname =
		["Create "+++varname]
	_ = []

diagnosedErrorToTask :: DiagnosedError String (Shared (EditorInfo,Map String [String])) -> Task ()
diagnosedErrorToTask (filename,line,_) "Go to error" editorStore = return ()
diagnosedErrorToTask (filename,line,UndefinedVar varname) _ editorStore = solutionCreateVar filename editorStore line varname
diagnosedErrorToTask (filename,line,_) actiona editorStore = viewInformation "" [] ("undefined action "+++ actiona) >>| return ()

getErrorInt :: String -> Int
getErrorInt err
	#(rest,err) = splitOnce "[" err
	#(errinfo,errmessage) = splitOnce "]" err
	#[filename,line,obj:rest] = split "," errinfo
	= (toInt line) - 1

diagnose :: String -> DiagnosedError
diagnose err
	#(rest,err) = splitOnce "[" err
	#(errinfo,errmessage) = splitOnce "]" err
	#[filename,line,obj:rest] = split "," errinfo
	#errwords = split " " (dropChars 2 errmessage)
	#line = (toInt line) - 1
	| length errwords == 2
		| indexOf "undefined" (errwords!!1) <> -1
			= (filename,line,UndefinedVar (errwords!!0))
		| otherwise = (filename,line,Unknown errmessage)
	| otherwise = (filename,line,Unknown errmessage)

showButtons :: String [(Action,Task ())] String (ReadOnlyShared (Maybe String)) (Task ()) -> Task ()
showButtons message l str smstr t = viewInformation "" [] message ||- watch smstr >>*
		[(OnValue (ifValue (\mstr. mstr <> (Just str)) \a.t)):(map (\(a,t). OnAction a (always t)) l)]

startOfFunction :: String Int -> Task Int
startOfFunction filepath i = contentLinesOf filepath >>- \l. return (sof l (i+1))
	where
	sof :: [String] Int -> Int
	sof l 0 = 0
	sof l i
		| (indexOf "::" (l!!i)) == -1 && (indexOf "Start" (l!!i)) == -1	=  sof l (i-1)
		| otherwise						= i

placeText :: String Int [String] -> Task ()
placeText filepath i text =
	contentLinesOf filepath >>- \l.
	setContent filepath ((\(a,b).a++text++b) (splitAt i l)) >>| return ()

jumpToLine :: (Shared (EditorInfo,Map String [String])) Int -> Task ()
jumpToLine editorstore i =
	upd (\(ei,c). ({ei & position = (i, snd ei.EditorInfo.position)},c)) editorstore >>| return ()

solutionCreateVar :: String (Shared (EditorInfo,Map String [String])) Int String -> Task ()
solutionCreateVar filename editorstore i var = filenameToFilepath filename >>- (\a. case a of
	Nothing = viewInformation "" [] "cannot find file" >>| return ()
	Just filepath =
		startOfFunction filepath i >>- \funi.
		placeText filepath funi [var+++" :: ",""] >>|
		jumpToLine editorstore funi
	)

//offerSolutions shows possible actions on selecting an error.
/*offerSolutions editorstore error = forever ((viewSharedInformation "" [] error ||- watch error) >>*
	[ OnValue (ifValue (\mstr. isJust mstr) (\(Just err). jumpToLine editorstore (getErrorInt err) >>| case diagnose err of
		UndefinedVar filename i var = sb "" [(Action ("Create "+++var), createVarSolution filename editorstore i var)] err
		Unknown filename i = sb filename [] err
		))])
	where
	sb message l err = showButtons message l err error (offerSolutions editorstore error)*/

/*structuur van een goedwerkende offerSolutions
os :: (ReadOnlyShared (Maybe String)) -> Task ()
os error = watch error >>*
	[OnValue (ifValue (\mstr. isJust mstr) (\(Just err). case diagnose err of
		Solvable args = (showButtons args ||- watch error) >>* [OnValue (ifValue \mstr. mstr <> (Just err)) (os)]
	)]
*/



build :: Task ()
build
			= 				get settings
			>>- \curSet ->	get project
			>>- \myProj ->	get contents
			>>- \c -> 		'DM'.foldrWithKey (\k v t -> t >>|- saveFile (k) (foldr joinWithNewline "" v)) (return ()) c
			//>>|-			get clooglestore
			//>>- \db -> 		accWorld (\w0. (\(tempdb,w).(finaliseDb tempdb db,w)) ('DM'.foldrWithKey (\k v (tempdb,w) .indexModule (""</>"") k ("") False tempdb w) (newTemporaryDb,w0) c))
			//>>- \db ->		set db clooglestore
			>>|-			updateDclStore
			>>- \dcls.		'DM'.foldrWithKey (\k v t -> t >>|- saveFile (changeExtension k "dcl") (toDcl k v)) (return ()) dcls
 			>>|-			compile (curSet.cleanHome </> "bin" </>"cpm") myProj.projectPath myProj.projectName
			>>|				readLinesFromFile (curSet.cleanHome </> errorFile)
			>>- \(merr) ->  case merr of
				Nothing = viewInformation "" [] "can't find errorfile" @! ()
				Just errors = set errors errorStore @! ()

changeExtension :: String String -> String
changeExtension filename ext = (dropExtension filename) +++ "." +++ ext

commentToFilecomment :: Comment -> String
commentToFilecomment c
# [linefirst:c] = split OS_NEWLINE c
# c = map (\line."*"+++line) c
# linefirst = "/** "+++linefirst
= (join OS_NEWLINE [linefirst:c])+++OS_NEWLINE+++"*/"


toDcl :: String [(Sharenum,String,String)] -> String
toDcl filename dcl
# dcl = filter (\(x,y,z).x === Sharedi || x === Sharedf) dcl
= join (OS_NEWLINE+++OS_NEWLINE) ["implementation module "+++(dropExtension (dropDirectory filename)):map (\(x,y,z).joinWithNewline (commentToFilecomment z) y ) dcl]

compile :: String String String   -> Task ()
compile cpmBin buildDir mainModule =
		callProcess "Building project" [] cpmBin [(buildDir </> mainModule) +++ ".prj"] (Just (""</>"")) @! ()


// ------------- parsing error messages from compiler

:: Error =  { kind 		:: String 		// error, info, parse error
			, file		:: String		// file name
			, line	 	:: (Int,Int)	// position
			, def		:: String		// kind of definition
			, info		:: String		// error message
			}

showError :: !(Shared (!AceOptions,!AceState)) !String  -> Task ()
showError  editorStore errorMessage
	# error = toError errorMessage
	= // viewInformation "error " [] error
		upd (\(options,state) -> (options,{state & cursor = error.line})) editorStore @! ()

toError :: String -> Error
toError errorMessage
	= 	{ kind 	= toString kind
		, file	= toString file
		, line	= (max (lno - 1) 0,max (cno - 1) 0)	// aceEditor column no start counting with 0
		, def	= toString def
		, info	= toString info
		}
where
	cno			= toInt (toString cn)
	lno 		= toInt (toString ln)
	(ln,cn)  	= mySpan ((<>) ';') pos
	(pos,def)  	= mySpan ((<>) ',') r1
	(file,r1)  	= mySpan ((<>) ',') position
	(kind,position) = mySpan ((<>) '[') before
	(before,info) 	= mySpan ((<>) ']') all

	mySpan pred list
		= case span pred list of
			(f,[]) -> (f,[])
			(f,s)  -> (f,tl s)

	all :: [Char]
	all = [c \\ c <-: errorMessage]
