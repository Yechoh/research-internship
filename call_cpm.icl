module call_cpm//import iTasksimport Text, Text.HTMLimport System.Processexamplecode = ""cpmloc = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\cpm.exe"cpmbuildloc = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship\\test1"pythonloc = "python"pybuildloc = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship"pyargs = ["-c","import foo; print foo.hello()"]pyfileloc = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship\\foo.py"javaloc = "C:\\Program Files\\Java\\jdk1.8.0_73\\bin\\java.exe"javafileloc = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship\\Hello" //werkt, maar geeft geen output. Zou een error als output moeten gevenStart world = callProcess javaloc [javafileloc] (Nothing) world//runt forever//Start world = startEngine (callProcess "de groeten" [] javaloc [javafileloc] (Just pybuildloc)) world//Start world = startEngine (createExec cpmloc cpmbuildloc "test1") worldcreateExec :: String String String   -> Task ()createExec cpmBin buildDir mainModule		= build @! ()where	build 										=		createCleanProject 		>>|		buildCleanProject  	createCleanProject   		= 	callProcess "Create project" [] cpmBin ["project",mainModule,"create"] (Just buildDir)		>>| setProjectOptions  			where		setProjectOptions 			=  				importTextFile projectFile			>>- \content -> exportTextFile projectFile (setOptions content)		projectFile = buildDir </> mainModule +++ ".prj"		setOptions s = ( replaceSubString "HeapSize:\t2097152" "HeapSize:\t20971520"		//			   o replaceSubString "Target:\tStdEnv" "Target:\tiTasks"		/*			   o replaceSubString "Stack:\tFalse" "Stack:\tTrue"*/) s	buildCleanProject   		= callProcess "Building project" [] cpmBin [mainModule +++ ".prj"] (Just buildDir)				