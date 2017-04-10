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
import iTasks._Framework.IWorld

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
		shortcuts :: [Shortcut],
		selection :: Maybe AceRange,
		position :: (Int,Int),
		theme :: String,
		readOnly :: Bool,
		prev_time :: Time
	}
	
derive class iTask EditorInfo, Shortcut

editorInfo = 
	{
		shortcuts = [],
		position = (0,0),
		selection = Nothing,
		theme = "",
		readOnly = False,
		prev_time = {Time|hour=0,min=0,sec=0}
	}
/*
editorRecord = 
	{
		content = [""],
		prev_time = iworldLocalTime,
		shortcuts = [],
		position = (0,0),
		selection_position = (0,0),
		theme = "",
		readOnly = False
	}
*/
pageEditor :: String String EditorRedirects -> Task ()
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
eiAndContents2ErRead :: String (EditorInfo,Map String [String])  -> (!AceOptions,!AceState)
eiAndContents2ErRead filename (ei,contents)  =
	(
		{AceOptions|
			theme = ei.EditorInfo.theme,
			mode = "C:\\Users\\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\research-internship\\ace\\clean.js"
		}
		,
		{AceState|
			lines = fromJust (fst ('DM'.getU filename contents)),
			cursor = ei.EditorInfo.position,
			selection = ei.EditorInfo.selection,
			disabled = ei.EditorInfo.readOnly
		}
	)

er2EiAndContentsWrite :: String (!AceOptions,!AceState) (EditorInfo,Map String [String]) -> Maybe (EditorInfo,(Map String [String]))
er2EiAndContentsWrite filename (ao,as) (ei,contents) =
	Just (
		{EditorInfo|
			shortcuts = ei.EditorInfo.shortcuts,
			selection = as.AceState.selection,
			position = as.AceState.cursor,
			theme = ao.AceOptions.theme,
			readOnly=as.AceState.disabled,
			prev_time=ei.EditorInfo.prev_time
		}, 
		'DM'.put filename as.lines contents
	)
		    
editor :: String -> Task (!AceOptions,!AceState)
editor name = withShared editorInfo (\ei. 
	updateSharedInformation (name) [UpdateUsing id (\_ nsc -> nsc) aceEditor] (er ei name))
	where
	er ei filename = mapReadWrite ((eiAndContents2ErRead filename), (er2EiAndContentsWrite filename)) (ei >*< contents)

		    
