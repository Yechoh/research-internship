implementation module errorHandling

import iTasks
import shares
import System.File
import directoryBrowsing
import _SystemArray
import extraTaskCombinators
import qualified Data.Map as DM

//!String !(Shared (!AceOptions,!AceState))

errorWindow :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
errorWindow nameOfFileInEditor editorStore
	=				\tasklist.get project
	>>- \myProj ->	accWorld (fileExists (myProj.projectPath </> myProj.projectName +++ ".exe"))
	>>- \isExe -> get contents 
	>>- \c -> 'DM'.foldrWithKey (\k v t -> t >>|- saveFile k (foldr joinWithNewline "" v)) (return ()) c
	>>|-	
		enterChoiceWithShared  (Title ("Errors & Warnings")) [ChooseFromGrid id] errorStore >>|- return ()
	/*>^*				[ OnAction (Action "/File/Open File") (ifValue (\err -> fileName err <> dropDirectory nameOfFileInEditor) (findAndOpenFile myProj))
					, OnAction (Action "Goto Error") 	  (ifValue (\err -> fileName err == dropDirectory nameOfFileInEditor) (showError editorStore))
					]
	>>*				[ OnAction (Action "/Project/Build")			(always  Build)
				 	, OnAction (Action "/Project/Run")				(ifCond  isExe RunProject)
					, OnAction (Action "/Project/Set Settings")		(always  jumpToSetSettings)
				 	]*/						
	/*where
		findAndOpenFile myProj errorMessage 
			= 				findFileInProjectEnv (fileName errorMessage)
			>>- \mbFound -> case mbFound of
								Nothing 				-> viewInformation "Cannot find file " [] (fileName errorMessage) @! ()
								Just (readOnly,path)	-> jumpToEditorPage readOnly False path*/

		/*fileName :: String -> String
		fileName errorMessage = {c \\ c <- (takeWhile ((<>) ',') (tl (dropWhile ((<>) '[' ) [c \\ c <-: errorMessage])))}

		RunProject  
			= 				get project 
			>>- \myProj ->	runExec (myProj.projectPath </> myProj.projectName +++ ".exe") 8080 @! ()*/

build :: Task ()
build 
			= 				get settings 
			>>- \curSet ->	get project
			>>- \myProj ->	compile (curSet.cpmDirectory </> cpmFile) myProj.projectPath myProj.projectName
			>>|				readLinesFromFile (curSet.cpmDirectory </> errorFile) 
			>>- \(Just errors) ->  set errors errorStore @! ()	
			
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