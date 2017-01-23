module call_cpm

import iTasks
import Text, Text.HTML

examplecode = ""
cpmloc = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\cpm.exe"
buildloc = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship\\test1"

Start world = startEngine (createExec cpmloc buildloc "test1") world

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