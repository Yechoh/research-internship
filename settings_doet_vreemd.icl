module settings_doet_vreemd

import iTasks

import iTasks.API.Extensions.Admin.WorkflowAdmin
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor.Builtin
import iTasks.API.Extensions.Editors.Ace
import StdFile, System.File
import System.Directory
import System.FilePath
import _SystemArray
import Data.Error
import qualified Data.Map as DM
//import PmDriver
import Text
import System.OS

//import PmProject

//import createAndRunExec
//import directoryBrowsing

//import CpmLogic2


import qualified Data.Map as DM
:: Settings = 	{ dirCpm 	:: FilePath
				, dirClean	:: FilePath
				, dirClean2 :: FilePath
				}
derive class iTask Settings

settings = sharedStore "settings" 	{ dirCpm = "C:/Users/Martin/Documents/clean-classic-itasks-windows-x86-20161223/cpm.exe"
									, dirClean = "C:/Users/Martin/Documents/clean-classic-itasks-windows-x86-20161223"
									, dirClean2 = "C:\Users\Martin\Documents\clean-classic-itasks-windows-x86-20161223"
									}									

									
Start :: *World -> *World
Start world = startEngine
	(viewInformation "" [] "ok1?" >>| 
	get settings >>= \settings2.
	viewInformation "" [] (settings2.dirClean2) >>|
	viewInformation "" [] "ok3?"
	)
	
	world