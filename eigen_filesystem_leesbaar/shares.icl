implementation module shares

import iTasks
import qualified Data.Map as DM
import System.OS
import iTasks.Extensions.Editors.Ace
import System.FilePath
import extraText
import StdArray
import builddb
import CloogleDB
import extraList

derive class iTask Settings, Project, Sharenum

instance == Project
	where
		(==) p1 p2 = p1.projectName == p2.projectName

// ------------- global stores

settings :: Shared Settings
settings 	= sharedStore "settings" 	{ cleanHome	= ""
										}
project :: Shared Project
project 	= sharedStore "project" 	{ projectName		= ""
										, projectPath 		= ""
										, projectSources	= []
										}

instance zero [(Sharenum,String,Comment)]
 	where
		zero = []

dclStore :: Shared (Map String [(Sharenum,String,Comment)])
dclStore = sharedStore "dclstore" 'DM'.newMap

:: Sharenum = Sharedf | Unsharedf | Deprecatedf | Sharedi | Unsharedi
//:: Comment :== {description :: String, args :: [String], result :: String }
:: Comment :== String

searchterm :: Shared (String)
searchterm = sharedStore "searchterm" ""

clooglestore :: Shared (CloogleDB)
clooglestore = sharedStore "clooglestore" zero

errorStore :: Shared [String]
errorStore = sharedStore "errorstore" []

contents :: Shared (Map String AceState)
contents = sharedStore "contents" 'DM'.newMap

instance zero AceState
	where
	zero = { lines             = []    //The lines of text in the editor
		, cursor            = (0,0) //The location of the cursor (<row>,<column>)
		, selection         = Nothing   //A text selection is delimited by this position and the cursor position
		, disabled          = False       //Disallow editing
		}

instance zero AceOptions
where
	zero = {
		theme = "",
		mode = ""
		}

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

derive class iTask Shortcut
