implementation module createAndRunExec

import iTasks
import Text, Text.HTML

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


compile :: String String String   -> Task ()
compile cpmBin buildDir mainModule
		= callProcess "Building project" [] cpmBin [mainModule +++ ".prj"] (Just buildDir) @! ()


// run an executable and view its output in the browser

runExec :: String Int   -> Task ()
runExec execPath portNumber  						
	=				callProcess "Executing" [ViewAs toView] execPath ["-port",toString portNumber] (Just (takeDirectory execPath)) @! ()
	>>= \status	->	return ()	
where
	toView _ = ATag [HrefAttr ("http://localhost:" +++ toString portNumber +++ "/"),TargetAttr "_blank"] 
					[Text ("View the code at: http://localhost:" +++ toString portNumber)]

/*

compileModule :: String String String String -> Task ()
compileModule cpmBin buildDir projectName moduleName
		= callProcess "Compile module" [] cpmBin ["module", moduleName,] (Just buildDir) @! ()
*/