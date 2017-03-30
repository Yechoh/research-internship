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

:: Shortcut = No_shortcut 
			| Ctrl_slash 
			| Ctrl_backslash 
			| Ctrl_Shift_backslash 
			| Ctrl_equals 
			| Ctrl_Shift_equals 
			| Ctrl_b //
			| Ctrl_d
			| Ctrl_Shift_d
			| Ctrl_e
			| Ctrl_Shift_e
			| Ctrl_i
			| Ctrl_l
			| Ctrl_m
			| Ctrl_n
			| Ctrl_o
			| Ctrl_r
			| Ctrl_Shift_r
			| Ctrl_s
			| Ctrl_Shift_s
			| Ctrl_Alt_s
			| Ctrl_Shift_Alt_s
			| Ctrl_w
			| Ctrl_Shift_w

:: EditorInfo = 
	{
		shortcuts = [Shortcut],
		selection_position = (Int,Int),
		position = (Int,Int),
		theme = String,
		highlighted = String,
		readOnly = Bool,
		prev_time = Time
	}

editorInfo = 
	{
		shortcuts = [],
		position = (0,0),
		selection_position = (0,0),
		theme = "",
		highlighted = "",
		readOnly = False,
		prev_time = now
	}

editorRecord = 
	{
		content = [""],
		prev_time = now,
		shortcuts = [],
		position = (0,0),
		selection_position = (0,0),
		theme = "",
		highlighted = "",
		readOnly = False
	}

pageEditor :: EditorRedirects -> Task ()
pageEditor path name ((actionQuit,pagenodeQuit),(actionAskImportPaths,pagenodeAskImportPaths)) =
	(editor name 
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
/*
latest :: Time Time -> Time
latest t1 t2
 | t1.hour > t2.hour = t1
 | t1.hour < t2.hour = t2
 | t1.min > t2.min = t1
 | t1.min < t2.min = t2
 | t1.sec > t2.sec = t1
 | otherwise = t2
*/
eiAndContents2ErRead :: (EditorInfo,Map String [String]) String -> EditorRecord
eiAndContents2ErRead (ei,contents) filename =
	{EditorRecord |
		content = getU filename contents,
		prev_time = ei.prev_time,
		shortcuts=ei.shortcuts,
		position=ei.position,
		selection_position=ei.selection_position,
		theme=ei.theme,
		highlighted=ei.highlighted,
		readOnly=ei.readOnly
	}

er2EiAndContentsWrite :: EditorRecord (EditorInfo,Map String [String]) String -> Maybe (EditorInfo,(Map String [String],Time))
er2EiAndContentsWrite er (ei,contents) filename =
	Just (
		{EditorInfo|
			shortcuts = er.shortcuts,
			selection_position = er.selection_position,
			position = er.position,
			theme = er.theme,
			highlighted=er.highlighted,
			readOnly=er.readOnly,
			prev_time=er.prev_time
		}, 
		put filename ei.content contents
	)
		    
editor :: String -> Task {#Char}
editor name = withShared editorInfo (\ei. 
	updateSharedInformation (name) [UpdateUsing id (\_ nsc -> nsc) aceTextArea] er
	where
	er filename = mapReadWrite (eiAndContents2ErRead filename) (er2EiAndContentsWrite filename) (ei >*< contents)

		    
