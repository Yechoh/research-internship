module webIDE

import iTasks

import iTasks.API.Extensions.Admin.WorkflowAdmin
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor.Builtin
import iTasks.API.Extensions.Editors.Ace
import StdFile, System.File
import System.Directory
import System.FilePath
import _SystemArray
import Data.Error
import qualified Data.Map as DM
import PmDriver
import Text
import System.OS

import PmProject

import createAndRunExec
import directoryBrowsing

import CpmLogic2

// -------------
import qualified Data.Map as DM
:: Settings = 	{ dirCpm 	:: FilePath
				, dirClean	:: FilePath
				, dirClean2 :: FilePath
				}
derive class iTask Settings

settings = sharedStore "settings" 	{ dirCpm = "C:/Users/Martin/Documents/clean-classic-itasks-windows-x86-20161223/cpm.exe"
									, dirClean = "C:/Users/Martin/Documents/clean-classic-itasks-windows-x86-20161223"
									, dirClean2 = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223"
									}
updSettings 
	= 		updateSharedInformation "Current Setting: " [] settings

errorstate = sharedStore "errors" ""
content = sharedStore "content" ""

// utility functions

isCleanFile file = isMember (takeExtension file) ["icl", "dcl", "prj", "abc", "sapl"]
isFile str file = (takeExtension file) == str

selectIcon pwd _ 	 = Nothing	// don't know where to store icons yet
/*selectIcon pwd "icl" = Just (pwd </> "WebPublic" </> "Clean.icl.ico")
selectIcon pwd "dcl" = Just (pwd </> "WebPublic" </> "Clean.dcl.ico")
selectIcon pwd "prj" = Just (pwd </> "WebPublic" </> "Clean.prj.ico")
selectIcon pwd "abc" = Just (pwd </> "WebPublic" </> "Clean.abc.ico")
selectIcon pwd _ 	 = Nothing*/

// missing operator hack for avoiding continues 

(>>|-) infixl 1 :: (Task a) (Task b) -> Task b | iTask a & iTask b
(>>|-) ma mb = ma >>- \_ -> mb

// -------------

Start :: *World -> *World
Start world = startEngine
	treeEdit
	//(readFromFile "jemoeder")
	//ideDashboard
	//showUnresolvedImports
	/*(
	viewInformation "" [] "ok1?" >>| 
	getPwdName >>= \pwd.
	get settings >>= \settings2.
	viewInformation "" [] (pwd+++settings2.dirClean2) >>|
	viewInformation "" [] "ok3?"
	)*/
	//(askImports "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship\\eigen filesystem\\EditedwebIDE.icl")
	world



// ------------- to do:

selectFromTreeWithOption :: String -> Task String
selectFromTreeWithOption pwd = selectFromTree pwd isCleanFile [OnAction (Action "Change paths")	(always ((updateSharedInformation "paths" [] settings) -&&- (viewInformation "" [] "The map research-internship is expected to be in the Itasks-SDK map in the libraries map in the clean directory") >>| selectFromTreeWithOption pwd))]

treeEdit :: Task ()//(({#Char},String),())
treeEdit 
	=(							getPwdName
 	>>- \pwd ->					selectFromTreeWithOption pwd
 	>>- \path ->				readFromFile path
	>>- \(Just contenttxt) -> 	readFromFile (toproj path) 
	>>- \mprojtxt ->			(case mprojtxt of
	Nothing	->					appWorld (createProject (pwd </> "Temp" </> (dropDirectory path)))
	(Just projtxt) -> 			writeToFile (pwd </> "Temp" </> (toproj (dropDirectory path))) projtxt >>| return ())
	 /*if (isJust mprojtxt) (writeToFile (pwd </> "Temp" </> (toproj (dropDirectory path))) (fromJust mprojtxt) >>| return ()) (appWorld (createProject (pwd </> "Temp" </> (dropDirectory path))))*/
	>>| 						set contenttxt content  
	>>|							editFile (takeDirectory path) (dropDirectory path) pwd)
/*
treeEdit :: Task ()//(({#Char},String),())
treeEdit 
	=							getPwdName
 	>>- \pwd ->					selectFromTree pwd isCleanFile []
 	>>* [   OnAction  ActionContinue   			(ifValue (isFile "icl") (\path -> openAndEdit path pwd))
	 	,	OnAction (Action "Change paths")	(always ((updateSharedInformation "paths" [] settings) -&&- (viewInformation "" [] "The map research-internship is expected to be in the Itasks-SDK map in the libraries map in the clean directory") >>| treeEdit))
	    ]
openAndEdit :: String String -> Task ()
openAndEdit path pwd =			
								readFromFile path
	>>- \(Just contenttxt) -> 	readFromFile (toproj path) 
	>>- \mprojtxt ->			(case mprojtxt of
	Nothing	->					appWorld (createProject (pwd </> "Temp" </> (dropDirectory path)))
	(Just projtxt) -> 			writeToFile (pwd </> "Temp" </> (toproj (dropDirectory path))) projtxt >>| return ())
	>>| 						set contenttxt content  
	>>|							editFile (takeDirectory path) (dropDirectory path) pwd
*/
/*
//# (exists,world)  = fileExists mainmodule world
createTempFiles :: String String -> Task (String,String)
createTempFiles path name =
	accWorld (getCurrentDirectory) >>=
	\cur. 
	(createFile (cur <\> "Temp" <\> name) "" -&&-
	 createFile (cur <\> "Temp" <\> replaceExtension name "prj")
*/

editor :: String String -> Task {#Char}
editor path name =
	updateSharedInformation (path </> name) [UpdateUsing id (\_ nsc -> nsc) aceTextArea] content

//updaters
//updateEverySecond :: 

//end updaters

cpmtask :: String -> Task ()
cpmtask iclloc = 
			get currentTime >>=
			\now.  waitForTime2 {Time| now & sec=now.Time.sec+1} >>- \_ ->
			(saveFile iclloc content) >>- \_ ->
			appWorld (compile prjloc) 
			>>- \_ -> readFromFile errorloc
			>>- \(Just errors). 
			set (errors) errorstate
			>>- \_ -> cpmtask iclloc
	where
	saveFile path content 
		= get content >>= \c. writeToFile path c @! ()
	prjloc = replaceExtension iclloc "prj"
	errorloc = replaceFileName iclloc "errors"

//(ifValue (\(a,now1).get currentTime >>= \now2. (now2.sec > now1.sec)))

waitForTime2 :: !Time -> Task Time
waitForTime2 time =
	watch currentTime >>* [OnValue (ifValue (\now -> time < now) return)]

Errors2Imports :: String -> String
Errors2Imports errors 
	# lines = split OS_NEWLINE errors
	# importlines = filter (\line. endsWith "imported" line) lines
	# importlinessplitted = map (split " ") importlines
	# dclnames = map (\x. hd (tl (tl x))) importlinessplitted
	=  join "\n" dclnames

import iTasks.UI.Editor.Builtin
import Text, Text.HTML, Data.List

showUnresolvedImports :: Task String
showUnresolvedImports = 
	viewSharedInformation "Unresolved imports"  /*[ViewUsing (\s -> SpanTag [] (intersperse (BrTag []) (map Text (split "\n" s)))) (htmlView 'DM'.newMap)] */[ViewUsing Errors2Imports (textArea 'DM'.newMap)] errorstate

addPath2Project :: String String String -> String
addPath2Project path cleandir projtxt
	# short_path = dropFileName (subString (size cleandir) (size path) path)
	# newpath = "\t\tPath:\t{Application}" +++ short_path
	# splitted_projtxt = split ("Path:\t{Project}"+++OS_NEWLINE) projtxt
	# paths = hd (tl splitted_projtxt)
	# newpaths = newpath +++ OS_NEWLINE +++ paths
	# newprojtxt = join ("Path:\t{Project}"+++OS_NEWLINE) [(hd splitted_projtxt),newpaths]
	= newprojtxt
	//= join "-" splitted_projtxt
	//= path
	//= projtxt

showMapSelector :: String -> Task ()
showMapSelector iclloc = 
	get settings
	>>= \settings. selectFromTree settings.dirClean2 (isFile "dcl") []
	>>= \path. readFromFile projloc 
	//>>- \projtxt. (viewInformation "" [ViewUsing id (textArea 'DM'.newMap)] (addPath2Project path settings.dirClean projtxt )) >>| return () 
	>>- \(Just projtxt). saveFile projloc (addPath2Project path settings.dirClean projtxt) "henk"
	>>- \_ -> appWorld (compile projloc) 
	>>- \_ -> readFromFile errorloc
	>>- \(Just errors). 
	set (errors) errorstate
	>>- \_ -> showMapSelector iclloc
	where
		projloc = toproj iclloc
		errorloc = replaceFileName iclloc "errors"

askImports :: String (Task ()) -> Task ()
askImports iclloc uppertask = 
	showUnresolvedImports
	||-
	showMapSelector iclloc
	>>* [   OnAction  ActionQuit    	(always (uppertask))
		]



editFile :: String String String -> Task () //(({#Char},String),())
editFile path name cur =
	 		(editor path name 
	 		-&&-
	 		viewSharedInformation "errors" [ViewUsing id (textArea 'DM'.newMap)] errorstate) 
	 		-&&-
	 		(cpmtask (cur </> "Temp" </> name))
	 >^*	[	OnAction  ActionSave    	(always (get content >>= \content. saveFile (path </> (name)) content (cur </> "Temp" </> name)))
	 		,	OnAction  ActionSaveAs		(always (get content >>= \content. saveFileAs (path </> name)content (cur </> "Temp" </> name))) 
	 		]
	 		++ 
	 		if (takeExtension name <> "icl") []
	 		[	OnAction (Action "Build")	(always (get content>>= \content. (const (buildProject path (dropExtension name)) content)))
	 		,	OnAction (Action "Run")		(always (runExec (path </> dropExtension name +++ ".exe") 8080))
	 			 		
	 		]
	 >>*	[   OnAction  ActionQuit    	(always (return ()))
	 		,	OnAction (Action "Import")	(always (askImports (cur </> "Temp" </> name) (editFile path name cur)))

		    ]
where	
	has_start content = {}
	
	saveFileAs path content temppath
		= 		updateInformation "Where to write to ?" [] path
		>>*		[	OnAction ActionSave		(hasValue (\name -> saveFile name content temppath))
			 	,	OnAction ActionCancel	(always (return ()))
			 	]
		
	buildProject buildPath iclFileName 
		= 					get settings 
		>>- \curSet ->		viewInformation "Calling cpm : " [] (curSet.dirCpm, buildPath, iclFileName)
		>>|					createExec curSet.dirCpm buildPath iclFileName

toproj :: String -> String
toproj path = replaceExtension path "prj"

remove_double_enters :: String -> String
remove_double_enters str = {c \\ c <-: str | c <> '\r'}

saveFile :: String String String -> Task ()
saveFile path content temppath = writeToFile path (remove_double_enters content) /*>>- ReadFromFile*/ @! () 



