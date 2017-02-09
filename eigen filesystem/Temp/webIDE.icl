module webIDE

import iTasks

import iTasks.API.Extensions.Admin.WorkflowAdmin
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor.Builtin
import iTasks.API.Extensions.Editors.Ace
import StdFile, System.File
import System.Directory
import System.FilePath
import Data.Error

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

// utility functions

isCleanFile file = isMember (takeExtension file) ["icl", "dcl", "prj", "abc", "sapl"]

selectIcon pwd _ 	 = Nothing	// don't know where to store icons yet
selectIcon pwd "icl" = Just (pwd </> "WebPublic" </> "Clean.icl.ico")
selectIcon pwd "dcl" = Just (pwd </> "WebPublic" </> "Clean.dcl.ico")
selectIcon pwd "prj" = Just (pwd </> "WebPublic" </> "Clean.prj.ico")
selectIcon pwd "abc" = Just (pwd </> "WebPublic" </> "Clean.abc.ico")
selectIcon pwd _ 	 = Nothing

// missing operator hack for avoiding continues 

(>>|-) infixl 1 :: (Task a) (Task b) -> Task b | iTask a & iTask b
(>>|-) ma mb = ma >>- \_ -> mb

// -------------

Start :: *World -> *World
Start world = startEngine (treeEdit) world
//Start world = startEngine ideDashboard world

// ------------- to do:

treeEdit :: Task ()
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

editor :: String String (RWShared .() {#.Char} {#.Char})-> Task {#Char}
editor path name sc =
	updateSharedInformation (path </> name) [UpdateUsing id (\_ nsc -> nsc) aceTextArea] sc

//updaters
//updateEverySecond :: 

//end updaters

cpmtask :: String (RWShared () {#Char} {#Char}) (RWShared () {#Char} {#Char}) -> Task ()
cpmtask iclloc content errs = 
			get currentTime >>=
			\now.  waitForTime {Time| now & sec=now.Time.sec+1} >>|
			saveFile iclloc content >>|
			appWorld (compile prjloc) 
			>>| readFromFile errorloc
			>>= \errors. 
			set ((toString now) +++ errors) errs
			>>| cpmtask iclloc content errs
	where
	saveFile path content 
		= get content >>= \c. writeToFile path c @! ()
	prjloc = replaceExtension iclloc "prj"
	errorloc = replaceFileName iclloc "errors"

//(ifValue (\(a,now1).get currentTime >>= \now2. (now2.sec > now1.sec)))


editFile :: String String String -> Task ()
editFile path name content =
	accWorld (getCurrentDirectory) >>=
	\(Ok cur). appWorld (createProject (cur </> "Temp" </> name)) >>|
	get currentTime >>=
	\now.withShared content
	(\sc. withShared (toString now)   
    (\errs ->
	 		(editor path name sc 
	 		-|| 
	 		viewSharedInformation "errors" [ViewUsing id aceTextArea] errs) -|| (cpmtask (cur </> "Temp" </> name) sc errs)
	 >^*	[	OnAction  ActionSave    	(hasValue (saveFile (path </> ("Edited" +++ name))))
	 		,	OnAction  ActionSaveAs		(hasValue (saveFileAs (path </> name)))
	 		]
	 		++ 
	 		if (takeExtension name <> "icl") []
	 		[	OnAction (Action "Build")	(hasValue (const (buildProject path (dropExtension name))))
	 		,	OnAction (Action "Run")		(always (runExec (path </> dropExtension name +++ ".exe") 8080))
	 		]
	 >>*	[   OnAction  ActionQuit    	(always (return ()))
		    ]
	))
where
	saveFile path content 
		= writeToFile path content @! () 
	
	saveFileAs path content 
		= 		updateInformation "Where to write to ?" [] path
		>>*		[	OnAction ActionSave		(hasValue (\name -> saveFile name content))
			 	,	OnAction ActionCancel	(always (return ()))
			 	]
		
	buildProject buildPath iclFileName 
		= 					get settings 
		>>- \curSet ->		viewInformation "Calling cpm : " [] (curSet.dirCpm, buildPath, iclFileName)
		>>|					createExec curSet.dirCpm buildPath iclFileName





