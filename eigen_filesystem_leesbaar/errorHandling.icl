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
import content

//!String !(Shared (!AceOptions,!AceState))





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
			>>- \c -> 		'DM'.foldrWithKey (\k v t -> t >>|- saveFile (k) (foldr joinWithNewline "" v.lines)) (return ()) c
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
= join (OS_NEWLINE+++OS_NEWLINE) ["definition module "+++(dropExtension (dropDirectory filename)):map (\(x,y,z).joinWithNewline (commentToFilecomment z) y ) dcl]

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
