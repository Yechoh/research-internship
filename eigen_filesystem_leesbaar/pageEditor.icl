implementation module pageEditor

import iTasks
import pagetypes
import shares
import extraTaskCombinators
import qualified Data.Map as DM
import callCpm
import directoryBrowsing
import createAndRunExec
import iTasks.API.Extensions.Editors.Ace
import iTasks.UI.Editor.Builtin

pageEditor :: String String EditorRedirects -> Task ()
pageEditor path name ((actionQuit,pagenodeQuit),(actionAskImportPaths,pagenodeAskImportPaths)) =
	(editor path name 
	 -&&-
	 viewSharedInformation "errors" [ViewUsing id (textArea 'DM'.newMap)] errorstate) 
	 -&&-
	 (repeatEverySecond (cpmSetErrorstate name))
	 >^*	[	OnAction  ActionSave    	(always (get content >>= \content. saveFile (path </> (name)) content))
	 		,	OnAction  ActionSaveAs		(always (get content >>= \content. saveFileAs (path </> name)content )) 
	 		]
	 		++ 
	 		if (takeExtension name <> "icl") []
	 		[	OnAction (Action "Run")		(always (runExec (path </> dropExtension name +++ ".exe") 8080))	
	 		]
	 >>*	[   OnAction  actionQuit   	(always pagenodeQuit)
	 		,	OnAction actionAskImportPaths	(always (pagenodeAskImportPaths path name))
		    ]
		    
editor :: String String -> Task {#Char}
editor path name =
	updateSharedInformation (path </> name) [UpdateUsing id (\_ nsc -> nsc) aceTextArea] content

		    
