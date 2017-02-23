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

import PmProject

import createAndRunExec
import directoryBrowsing

import CpmLogic2

// -------------
import qualified Data.Map as DM
:: Settings = 	{ dirCpm 	:: FilePath
				, dirClean	:: FilePath
				}
derive class iTask Settings

settings = sharedStore "settings" 	{ dirCpm = "C:/Users/Martin/Documents/clean-classic-itasks-windows-x86-20161223/cpm.exe"
									, dirClean = "C:/Users/Martin/Documents/clean-classic-itasks-windows-x86-20161223"
									}
updSettings 
	= 		updateSharedInformation "Current Setting: " [] settings

errorstate = sharedStore "errors" ""
content = sharedStore "content" ""

// utility functions

isCleanFile file = isMember (takeExtension file) ["icl", "dcl", "prj", "abc", "sapl"]
isFile file str = (takeExtension file) == str

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
Start world = startEngine (treeEdit) world
//Start world = startEngine ideDashboard world

// ------------- to do:

treeEdit :: Task ()//(({#Char},String),())
treeEdit 
	=							getPwdName
 	>>= \pwd ->					selectFromTree pwd isCleanFile 
	>>= \path ->				viewInformation "selected file" [] path
	>>= \path ->				readFromFile path
	>>- \content -> 			editFile (takeDirectory path) (dropDirectory path) content

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
			>>- \errors. 
			set (/*(toString now) +++ */errors) errorstate
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
	# lines = split errors "\n"
	# importlines = filter (\line. not (endsWith "imported" line)) lines
	# importlinessplitted = map (split " ") importlines
	# dclnames = /*map (\x. hd (tl (tl x)))*/ map hd importlinessplitted
	=  join "\n" dclnames

showUnresolvedImports :: Task String
showUnresolvedImports = 
	viewSharedInformation "Unresolved imports" [ViewUsing Errors2Imports (textArea 'DM'.newMap)] errorstate

addPath2Project :: String String -> String
addPath2Project path projtxt
	# path_without_cleandir = subString ((size settings.dirClean) (size path) path)
	# newpath = "{Application}" +++ path
	# splitted_projtxt = split projtxt "Path:\t{Project}\n\t\t"
	# paths = hd (tl splitted_projtxt)
	# newpaths = path +++ "\n\t\t" +++ paths
	# newprojtxt = join "Path:\t{Project}\n\t\t" [(hd splitted_projtxt),newpaths]
	= newprojtxt

showMapSelector :: String -> Task ()
showMapSelector iclloc = 
	get settings
	>>- \settings. selectFromTree settings.dirClean (isFile "dcl") 
	>>= \path. readFromFile projloc 
	>>- \projtxt. writeToFile path (addPath2Project path projtxt)
	>>- \_ -> appWorld (compile projloc) 
	>>- \_ -> readFromFile errorloc
	>>- \errors. 
	set (errors) errorstate
	>>- \_ -> showMapSelector iclloc
	where
		projloc = toproj iclloc
		errorloc = replaceFileName iclloc "errors"

AskImports :: String -> Task ()
AskImports iclloc = 
	showUnresolvedImports
	||-
	showMapSelector iclloc
	>>* [   OnAction  ActionQuit    	(always (return ()))
		]

editFile :: String String String -> Task () //(({#Char},String),())
editFile path name contenttxt =
	accWorld (getCurrentDirectory) >>=
	\(Ok cur). appWorld (createProject (cur </> "Temp" </> name)) >>|
	set contenttxt content  >>|
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
	 		,	OnAction (Action "Import")	(always (AskImports (cur </> "Temp" </> name)))
		    ]
where
	saveFile path content temppath = writeToFile path (remove_double_enters content) /*>>- ReadFromFile*/ @! () 
	
	toproj path = replaceExtension path "prj"
	
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



