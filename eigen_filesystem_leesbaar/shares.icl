implementation module shares

import iTasks
import qualified Data.Map as DM
import System.OS
import iTasks.API.Extensions.Editors.Ace
import System.FilePath
import Text
import StdArray

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
contentLinesOf filepath = 
	get contents >>- \c.
	case (fst ('DM'.getU filepath c )) of 
		Nothing = viewInformation "not found" [] (filepath,c) >>| return ["not found!"]
		Just lines = return lines
	
setContent :: String [String] -> Task (Map String [String])
setContent filepath content = upd (\c. 'DM'.put filepath content c) contents
	
joinWithNewline :: String String -> String
joinWithNewline a b = a+++OS_NEWLINE+++b

remove_newlines :: String -> String
remove_newlines str = {c \\ c <-: str | c <> '\n' && c <> '\r'}
	
contentOf :: String -> Task String
contentOf filename = get contents >>- \c.
	return (foldr (\a. joinWithNewline (remove_newlines a)) "" (fromJust (fst ('DM'.getU filename c ))))

/*contents uses the complete filepath as filename. 
This is because a user is allowed to have two files open with the same name but on different locations.
But sometimes we only know the filename.
This function finds the corresponding filepath*/
filenameToFilepath :: String -> Task (Maybe String)
filenameToFilepath filename =
	get contents >>- \c.
	case ('DM'.mapSize ('DM'.filterWithKey (\k a. takeFileName k == filename) c)) of
		1 = return (Just (hd ('DM'.keys ('DM'.filterWithKey (\k a. takeFileName k == filename) c))))
		0 = return Nothing
		a = enterChoice "Multiple files found with this name. Choose one:" [] ('DM'.keys ('DM'.filterWithKey (\k a. takeFileName k == filename) c))
			>>* [	OnAction ActionCancel (always (return Nothing))
				,	OnAction (Action "Choose") (hasValue \name. return (Just name))
				]
	
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
