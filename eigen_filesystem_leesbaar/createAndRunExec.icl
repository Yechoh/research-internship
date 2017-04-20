implementation module createAndRunExec

import iTasks
import Text, Text.HTML

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
					
					



