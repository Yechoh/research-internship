definition module ideStores

import iTasks

:: Settings 	= 	{ cpmDirectory 		:: FilePath			// directory of cpm.exe
					}
:: Environment 	=	{ environmentName 	:: String			// name of the environment
					, environmentLibs 	:: [Directory]		// cashed, read only libraries attached to it
					}
:: Directory 	= 	Dir !FileName ![Directory] ![FileName]
:: Project	 	=	{ projectName  		:: String			// name of the project (without .prj extension)
					, projectPath  		:: FilePath			// directory where project is stored
					, projectSources	:: [FilePath]		// not cashed, all file names read in after every cycle
					}
:: FileName 	:== String 									// File name without path, but with extension

derive class iTask Settings, Project, Environment, Directory 

instance == Directory, Project
instance <  Directory

// ------------- global stores

settings 		:: Shared Settings
environment 	:: Shared Environment
project 		:: Shared Project

recentFiles 	:: Shared [(!Bool,!FilePath)]
recentProjects	:: Shared [Project]
errorStore 		:: Shared [String]

parmStore 		:: Shared (Maybe (!Bool,!FilePath))			// temp hack to pass info from one page to another
	
// ------------- store access utilities

addToRecentFiles 	:: !(!Bool,!FilePath) 	-> Task ()
addToRecentProjects :: !Project  			-> Task ()

// ------------- show & set current settings of Project, Settings, & Environment,

showSettings 		:: Task () 
setSettings 		:: Task ()

// ------------- find file in Project or Environment

findFileInProjectEnv :: String -> Task (Maybe (Bool,FilePath))
