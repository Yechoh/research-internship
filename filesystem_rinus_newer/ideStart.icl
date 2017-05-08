module ideStart

import iTasks
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks._Framework.Tonic
import ideStores, ideUtil, ideEditor

// ------------- 

Start :: *World -> *World
Start world 
	= startEngine	[ publish "/" 				(\_ -> startTask)			// start here
					, editorPage											// open an editor in a browser page
					, publish "/set"  			(\_ -> forever setSettings)	// set project settings page
					, publish "/show"  			(\_ -> showSettings)		// show project settings and evironment paths
					, publish "/tonic"  		(\_ -> tonicDashboard [])	// to spect tonic blueprints, works only if tonic compiler is used
					, publish "/evalTask"  		evalMyTask					// start up task in a new browser window
					] world

// first force settings after which by default we open an editor onn the main .icl file
startTask :: Task ()
startTask 
	=				get settings
	>>- \curSet ->	get project
	>>- \curProj ->	let isProj = curProj.projectPath <> "" && curProj.projectName <> ""
					    isCPM  = curSet.cpmDirectory <> ""
					in if (not isProj || not isCPM) 
							(setSettings  >>| startTask)
					   		(editFile False (curProj.projectPath </> curProj.projectName +++ ".icl")) 
