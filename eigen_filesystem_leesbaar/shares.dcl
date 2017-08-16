definition module shares

import iTasks
import System.FilePath
import qualified Data.Map as DM
import iTasks.Extensions.Editors.Ace
import StdString, StdList
import CloogleDB
import CloogleDBFactory

:: Settings 	= 	{ cleanHome 		:: FilePath			// directory of cpm.exe
					}
:: Project	 	=	{ projectName  		:: String			// name of the project (without .prj extension)
					, projectPath  		:: FilePath			// directory where project is stored
					, projectSources	:: [FilePath]		// not cashed, all file names read in after every cycle
					}

derive class iTask Settings, Project, Sharenum

dclStore :: Shared (Map String [(Sharenum,String,Comment)])

updateDclStore :: Task (Map String [(Sharenum,String,Comment)])

:: Sharenum = Sharedf | Unsharedf | Deprecatedf | Sharedi | Unsharedi

searchterm :: Shared (String)
clooglestore :: Shared (CloogleDB)

settings 		:: Shared Settings
project 		:: Shared Project
errorStore :: Shared [String]

//contents = ([(filename,[line])],prev_time)
contents :: Shared (Map String [String])

:: Comment :== String

//functions to get specific content
contentLinesOf :: String -> Task [String]
setContent :: String [String] -> Task (Map String [String])
joinWithNewline :: String String -> String
contentOf :: String -> Task String
filenameToFilepath :: String -> Task (Maybe String)

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

cpmFile			 :== "cpm.exe"
cleanIDE		 :== "CleanIDE.exe"
errorFile		 :==  "Temp" </> "errors"

isCPM       	file 	:== file == cpmFile
isCleanIcl  	file 	:== takeExtension file == "icl"
isCleanIclDcl 	file 	:== isMember (takeExtension file) ["icl", "dcl"]
anyFile 		file	:== True
