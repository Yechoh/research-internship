implementation module errorHandling

import iTasks
import shares
import System.File
import directoryBrowsing
import _SystemArray
import extraTaskCombinators
import qualified Data.Map as DM
import Text
import doubleEnterChoice

//!String !(Shared (!AceOptions,!AceState))

errorWindow :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
errorWindow nameOfFileInEditor editorStore
	= \tasklist. doubleEnterChoiceWithShared "" [ChooseFromGrid id] errorstore 
	
		((enterChoiceWithShared "" [ChooseFromGrid id] errorStore) 
	>&>
	(\a.(viewSharedInformation "" [] a ||- (offerSolutions editorStore a)))) <<@ ApplyLayout (arrangeHorizontal) >>|- return () ) <<@ (Title ("Errors & Warnings"))  

/*showErrorsAndSolutions :: (Shared (EditorInfo,Map String [String])) -> Task ()
showErrorsAndSolutions editorStore = 
	(enterChoiceWithShared  (Title ("Errors & Warnings")) [ChooseFromGrid id] errorStore 
	>&^
	\choice.((showErrorsAndSolutions editorStore) ||- (offerSolutions editorStore choice)))  <<@ ApplyLayout (arrangeHorizontal)
*/	

		/*fileName :: String -> String
		fileName errorMessage = {c \\ c <- (takeWhile ((<>) ',') (tl (dropWhile ((<>) '[' ) [c \\ c <-: errorMessage])))}

		RunProject  
			= 				get project 
			>>- \myProj ->	runExec (myProj.projectPath </> myProj.projectName +++ ".exe") 8080 @! ()*/

:: LineNr :== Int
:: DiagnosedError = (Filename,LineNr,ErrorDiagnosis)
:: ErrorDiagnosis = UndefinedVar String | Unknown

diagnosedErrorToDescription :: DiagnosedError -> String
diagnosedErrorToDescription
| _ = ""

diagnosedErrorToSolutions :: DiagnosedError (Shared (EditorInfo,Map String [String])) -> [(String,Task ())]
diagnosedErrorToSolutions (filename,linenr,diagnosis) editorstore = //jumpToLine editorstore (getErrorLineNr err) >>|
	case diagnosis of
	| UndefinedVar varname = 
		[(Action ("Create "+++varname),solutionCreateVar filename editorstore linenr varname)]
	| _ = []

splitOnce :: !String !String -> (String,String)
splitOnce sep s 
	# index = indexOf sep s
	= (s%(0,index-1),s%(index+1,size s-1))

getErrorLineNr :: String -> Int
getErrorLineNr err 
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
			= UndefinedVar filename line (errwords!!0)
		| otherwise = Unknown (filename+++" "+++errwords!!1) line
	| otherwise = Unknown (filename) line

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
offerSolutions ::(Shared (EditorInfo,Map String [String])) (ReadOnlyShared (Maybe String)) -> Task ()
offerSolutions editorstore error = forever ((viewSharedInformation "" [] error ||- watch error) >>*
	[ OnValue (ifValue (\mstr. isJust mstr) (\(Just err). jumpToLine editorstore (getErrorLineNr err) >>| case diagnose err of
		UndefinedVar filename i var = sb "" [(Action ("Create "+++var), createVarSolution filename editorstore i var)] err
		Unknown filename i = sb filename [] err
		))])
	where
	sb message l err = showButtons message l err error (offerSolutions editorstore error) 

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
			>>- \c -> 'DM'.foldrWithKey (\k v t -> t >>|- saveFile k (foldr joinWithNewline "" v)) (return ()) c
			>>|- 			compile (curSet.cpmDirectory </> cpmFile) myProj.projectPath myProj.projectName
			>>|				readLinesFromFile (curSet.cpmDirectory </> errorFile) 
			>>- \(merr) ->  case merr of
				Nothing = viewInformation "" [] "can't find errorfile" @! () 
				Just errors = set errors errorStore @! ()	
			
compile :: String String String   -> Task ()
compile cpmBin buildDir mainModule
		= callProcess "Building project" [] cpmBin [mainModule +++ ".prj"] (Just buildDir) @! ()
	

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