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

instance zero AceState
instance zero AceOptions

derive class iTask Settings, Project, Sharenum

dclStore :: Shared (Map String [(Sharenum,String,Comment)])

:: Sharenum = Sharedf | Unsharedf | Deprecatedf | Sharedi | Unsharedi

searchterm :: Shared (String)
clooglestore :: Shared (CloogleDB)

settings 		:: Shared Settings
project 		:: Shared Project
errorStore :: Shared [String]

//contents = ([(filename,[line])],prev_time)
//contents :: Shared (Map String [String])
contents :: Shared (Map String AceState)

:: Comment :== String

instance zero [(Sharenum,String,Comment)]

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

cpmFile			 :== "cpm.exe"
cleanIDE		 :== "CleanIDE.exe"
errorFile		 :==  "Temp" </> "errors"

isCPM       	file 	:== file == cpmFile
isCleanIcl  	file 	:== takeExtension file == "icl"
isCleanIclDcl 	file 	:== isMember (takeExtension file) ["icl", "dcl"]
anyFile 		file	:== True
