implementation module shares

import iTasks
import qualified Data.Map as DM
import System.OS
import iTasks.Extensions.Editors.Ace


derive class iTask Settings, Project

instance == Project
	where
		(==) p1 p2 = p1.projectName == p2.projectName	

// ------------- global stores

settings :: Shared Settings
settings 	= sharedStore "settings" 	{ cpmDirectory	= ""
										} 
project :: Shared Project
project 	= sharedStore "project" 	{ projectName		= ""
										, projectPath 		= ""
										, projectSources	= [] 
										}

errorStore :: Shared [String]								
errorStore = sharedStore "errorstore" []

contents :: Shared (Map String [String])
contents = sharedStore "contents" 'DM'.newMap

contentLinesOf :: String -> Task [String]
contentLinesOf filename = 
	get contents >>- \c.
	return (fromJust (fst ('DM'.getU filename c )))
	
joinWithNewline :: String String -> String
joinWithNewline a b = a+++OS_NEWLINE+++b
	
contentOf :: String -> Task String
contentOf filename = get contents >>- \c.
	return (foldr joinWithNewline "" (fromJust (fst ('DM'.getU filename c ))))
	
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
