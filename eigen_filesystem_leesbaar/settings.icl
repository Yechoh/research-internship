implementation module settings

import System.FilePath, System.Environment, Text.HTML
import directoryBrowsing
import shares
import extraTaskCombinators
import qualified Data.Map as DM
import Text
import iTasks.Extensions.Process
import iTasks.Extensions.TextFile

setSettings :: Task ()
setSettings = setSettings` <<@ ApplyLayout frameCompact
where
	setSettings`
		=				accWorld (getEnvironmentVariable "CLEAN_HOME")
		>>- \mbHome ->  upd (\cs -> {cs & cleanHome = if (cs.cleanHome == "" && isJust mbHome) ((fromJust mbHome)) cs.cleanHome}) settings
		>>|				get settings
		>>- \curSet ->	get project
		>>- \recProj -> let isProj 			= recProj.projectPath <> "" && recProj.projectName <> ""
							isCPM  			= curSet.cleanHome <> ""
							projectPath 	= recProj.projectPath </> recProj.projectName +++ ".prj"
							applicationPath = curSet.cleanHome
						in	(if (not isCPM)  (viewInformation () [] "Where is the Clean home directory...")
							(if (not isProj) (viewInformation () [] ("The Clean home directory currently set is: " +++ curSet.cleanHome )
											 -||-
											 viewInformation () [] "No Project Set...")
					   			(viewInformation () [] "Change Settings..")
							))
							>>*		[ OnAction (Action "Set Clean home")	(always 		(mbCancel findHome <<@ ApplyLayout frameCompact))
							 		, OnAction (Action "Set Project")		(ifCond isCPM   (mbCancel NewProject <<@ ApplyLayout frameCompact))
							 		]

	findHome
		= 					viewInformation "" [] "where is the Clean home directory?"
		||-					getPwdName
 		>>- \pwd ->			selectFolder pwd
		>>- \(path,file) ->	upd (\curSet -> {curSet & cleanHome  = path}) settings

	NewProject
		= 					askUserForFile "the Clean .icl main file" isCleanIcl
		>>- \(path,file) -> upd (\myProj -> { myProj 	& projectPath 		= path
														, projectName 		= dropExtension file
														, projectSources 	= [path]
				  							}) project
		>>|					get settings
		>>- \curSet ->		get project
		>>- \myProj ->		createProject (curSet.cleanHome </> "bin" </> "cpm") "/" (myProj.projectPath </> myProj.projectName)
		>>|- setContents (path </> file)

setContents :: String -> Task ()
setContents iclloc
	= 							readLinesFromFile iclloc
	>>- \(Just contenttxt) ->	get contents
	>>- \contentmap ->			set (setText iclloc contenttxt contentmap) contents
								>>|- return ()

setText :: String [String] (Map String AceState) -> Map String AceState
setText iclloc contenttxt contentmap = 'DM'.put iclloc {zero & lines=contenttxt} contentmap

mbCancel :: (Task a) -> Task ()
mbCancel ta
	=
	(		(ta @! ())
	-||- 	(	viewInformation () [] ()
			>>*	[OnAction ActionCancel (always (return ()))]
			)
	)
	>>|- return ()

createExec :: String String String   -> Task ()
createExec cpmBin buildDir mainModule
		= build @! ()
where
	build
		=		createCleanProject
		>>|		buildCleanProject

	createCleanProject
		= 	callProcess "Create project" [] cpmBin ["project",mainModule,"create"] (Just buildDir)
		>>| setProjectOptions
	where
		setProjectOptions
			=  				importTextFile projectFile
			>>- \content -> exportTextFile projectFile (setOptions content)

		projectFile = buildDir </> mainModule +++ ".prj"

		setOptions s = ( replaceSubString "HeapSize:\t2097152" "HeapSize:\t20971520"
		//			   o replaceSubString "Target:\tStdEnv" "Target:\tiTasks"
		/*			   o replaceSubString "Stack:\tFalse" "Stack:\tTrue"*/) s

	buildCleanProject
		= callProcess "Building project" [] cpmBin [mainModule +++ ".prj"] (Just buildDir)

// run an executable and view its output in the browser

runExec :: String Int   -> Task ()
runExec execPath portNumber
	=		callProcess "Executing" [ViewAs toView] execPath ["-port",toString portNumber] (Just (takeDirectory execPath)) @! ()
where
	toView _ = ATag [HrefAttr ("http://localhost:" +++ toString portNumber +++ "/"),TargetAttr "_blank"]
					[Text ("View the code at: http://localhost:" +++ toString portNumber)]

createProject :: String String String   -> Task ()
createProject cpmBin buildDir mainModule
		= build @! ()
where
	build
		=		createCleanProject

	createCleanProject
		= 	callProcess "Create project" [] cpmBin ["project",mainModule,"create"] (Just buildDir)
		>>| setProjectOptions
	where
		setProjectOptions
			=  				importTextFile projectFile
			>>- \content -> exportTextFile projectFile (setOptions content)

		projectFile = buildDir </> mainModule +++ ".prj"

		setOptions s = ( replaceSubString "HeapSize:\t2097152" 	"HeapSize:\t20971520"
					   o replaceSubString "Target:\tStdEnv" 	"Target:\tiTasks"
					   o replaceSubString "Stack:\tFalse" 		"Stack:\tTrue"
					   o replaceSubString "Dynamics:\tFalse" 	"Dynamics:\tTrue"
					   o replaceSubString "Time:\tFalse" 	    "Time:\tTrue"	) s
