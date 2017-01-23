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

// -------------

import qualified Data.Map as DM
:: Settings = 	{ dirCpm 	:: FilePath
				, dirClean	:: FilePath
				}
derive class iTask Settings

settings = sharedStore "settings" 	{ dirCpm = "D:/Clean/iTasks-SDK/Examples/temp/cpm.exe"
									, dirClean = "D:/Clean"
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
Start world = startEngine (manageWorklist ide) world
//Start world = startEngine ideDashboard world

// ------------- to do:

ideDashboard :: Task ()
ideDashboard
	=  	forever treePanel <<@ ApplyLayout layout
where
	treePanel = treeEdit2

	layout = sequenceLayouts
		[ arrangeWithSideBar 0 LeftSide 260 True
		]

treeEdit2 :: Task ()
treeEdit2 
	=				getPwdName
 	>>= \pwd ->		selectFromTree pwd isCleanFile
//	>>= \path ->	viewInformation "selected fule" [] path
	>^*				[ 	OnAction  (Action "Open") (hasValue edit2)
	 				]
	>>|				return ()
where
 edit2 path
	=				readFromFile path
	>>= \content ->	appendTopLevelTask ('DM'.fromList [("title","malcom x")]) True (editFile path (dropDirectory path) content)
						 <<@ ToWindow FloatingWindow AlignMiddle AlignCenter
	>>|-			return ()

// ------------- ide try outs

ide :: [Workflow]
ide =
	[ workflow "Brose & Edit"		"Browse and Edit a file" 		browseEdit
	, workflow "Tree browse & Edit"	"Browse and Edit a file" 		treeEdit
	, workflow "Settings"			"View and change settings"  	updSettings

	, workflow "Browse"				"Browse through file system" 	(browseDirectory isCleanFile <<@ ApplyLayout (toWindow FloatingWindow AlignTop AlignLeft))
	]

treeEdit :: Task ()
treeEdit 
	=							getPwdName
 	>>= \pwd ->					selectFromTree pwd isCleanFile 
	>>= \path ->				viewInformation "selected fule" [] path
	>>= \path ->				readFromFile path
	>>- \content -> 			editFile path (dropDirectory path) content

browseEdit :: Task ()
browseEdit 
	=							browseDirectory isCleanFile
	>>- \((path,_),mbName) -> 	
	if (isNothing mbName)		(return ())
	(							readFromFile (path </> fromJust mbName)
	>>- \content -> 			editFile path (fromJust mbName) content
	)

// very simple editor

editFile :: String String String -> Task ()
editFile path name content 
= withShared content 
    (\sc ->
	 		updateSharedInformation (path </> name) [UpdateUsing id (\_ nsc -> nsc) aceTextArea] sc				// seems to be a bug in the current UpdateUsing ... no change returned

	 >^*	[ 	OnAction  ActionSave    	(hasValue (saveFile (path </> name)))
	 		,	OnAction  ActionSaveAs		(hasValue (saveFileAs (path </> name)))
	 		]
	 		++ 
	 		if (takeExtension name <> "icl") []
	 		[	OnAction (Action "Build")	(hasValue (buildProject path (dropExtension name)))
	 		,	OnAction (Action "Run")		(always (runExec (path </> dropExtension name +++ ".exe") 8080))
	 		]
	 >>*	[   OnAction  ActionQuit    	(always (return ()))
		    ]
	)
where
	saveFile path content 
		= writeToFile path content @! () 
	
	saveFileAs path content 
		= 		updateInformation "Where to write to ?" [] path
		>>*		[	OnAction ActionSave		(hasValue (\name -> saveFile name content))
			 	,	OnAction ActionCancel	(always (return ()))
			 	]
		
	buildProject buildPath iclFileName _
		= 					get settings 
		>>- \curSet ->		createExec curSet.dirCpm buildPath iclFileName





