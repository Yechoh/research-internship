implementation module ideStores

import iTasks
import System.FilePath, System.Environment, Text.HTML
import ideConstants, ideUtil, directoryBrowsing, createAndRunExec, projectOptions

derive class iTask Settings, Project, Environment, Directory 

instance == Directory
	where
		(==) (Dir name1 _ _) (Dir name2 _ _) = name1 == name2	

instance < Directory
	where
		(<) (Dir name1 _ _) (Dir name2 _ _) = name1 < name2	

instance == Project
	where
		(==) p1 p2 = p1.projectName == p2.projectName	

// ------------- global stores

settings :: Shared Settings
settings 	= sharedStore "settings" 	{ cpmDirectory	= ""
										} 
environment :: Shared Environment
environment	= sharedStore "evironment" 	{ environmentName = ""
										, environmentLibs = []
										}
project :: Shared Project
project 	= sharedStore "project" 	{ projectName		= ""
										, projectPath 		= ""
										, projectSources	= [] 
										}

recentFiles :: Shared [(!Bool,!FilePath)]
recentFiles = sharedStore "recentFiles" []

parmStore :: Shared (Maybe (!Bool,!FilePath))							// temp hack to pass info from one page to another
parmStore  = sharedStore "parameterPassing" Nothing

recentProjects :: Shared [Project]
recentProjects = sharedStore "recentProjects" []


addToRecentFiles :: !(!Bool,!FilePath) -> Task ()
addToRecentFiles file = upd (\rf -> take 10 (removeDup [file:rf])) recentFiles @! ()

addToRecentProjects :: !Project -> Task ()
addToRecentProjects project	= upd (\rp -> take 10 (removeDup [project:rp])) recentProjects@! ()

errorStore :: Shared [String]
errorStore = sharedStore "lastErrors" []

// ------------------------

showSettings :: Task () 
showSettings 
	= forever
	(				get recentFiles
	>>- \recent ->	get project
	>>- \curProj ->	get environment
	>>- \curEnv ->
		(		anyTask
				[ viewInformation (Title (curProj.projectName +++ ".prj")) [] "" @! ()
				, viewDirectory curProj.projectPath (openSelected False) (mapNotCashed curProj.projectSources) @! ()
				, viewDirectory curEnv.environmentName (openSelected True) (mapCashed curEnv.environmentLibs) @! ()
				] 
		>>* 	[ OnAction (Action "/Project/Add Path")	(always	(mbCancel addProjectPath))
				, OnAction (Action "/Env/Add Path")	(always	(mbCancel addEnvironmentPath))
			 	]
		)
	)
where
	mapCashed dirs 		= map (\dir -> Cashed dir) dirs
	mapNotCashed dirs 	= map (\dir -> NotCashed dir anyFile) dirs

	openSelected :: Bool  -> [TaskCont FilePath (Task())]
	openSelected True  
		= 		[ OnAction ActionOpen 	(ifValue isCleanIclDcl (jumpToBrowserEditor True True))
				]  
	openSelected False  
		= 		[ OnAction ActionOpen 	(ifValue isCleanFile (jumpToBrowserEditor True False))
				, OnAction ActionDelete	(hasValue deleteThisFile)
				]  

	addProjectPath
		=						askUserForFile "new project path" isCleanIclDcl
		>>- \(path,file) ->		upd (\myProj -> {myProj & projectSources =  sort(removeDup [path</>file:myProj.projectSources])}) project

	addEnvironmentPath
		=						askUserForFile "additional library path" isCleanIclDcl
		>>- \(path,file) ->		fetchDirectories path isCleanIclDcl
		>>- \dirs ->			upd (\curEnv -> {curEnv & environmentLibs =  sort(removeDup [dirs:curEnv.environmentLibs])}) environment

jumpToBrowserEditor :: !Bool !Bool !FilePath -> Task ()
jumpToBrowserEditor newPage readOnly fileName 
	= 		jumpToBrowserEditor` readOnly fileName <<@ InModalDialog @! () 
where
	jumpToBrowserEditor` readOnly fileName
		=	set (Just (readOnly,fileName)) parmStore									// store parameter for to be read by page called
		>>|	mbCancel
				(		viewInformation "Click on link to open editor: " [] (hopToEditor fileName) 
				||-		wait () (\parm -> isNothing parm) parmStore @! ()				// wait until parameter has been read by called page
				)
		>>| return ()

	hopToEditor text = (ATag [HrefAttr "/editor", TargetAttr (if newPage "_blank" "_inline")] [Text text])

// ---------------

setSettings :: Task ()
setSettings = setSettings` <<@ ApplyLayout frameCompact

where 
	setSettings`
		=				accWorld (getEnvironmentVariable "CLEAN_HOME")
		>>- \mbHome ->  upd (\cs -> {cs & cpmDirectory = if (cs.cpmDirectory == "" && isJust mbHome) (fromJust mbHome) cs.cpmDirectory}) settings 
		>>|				get settings
		>>- \curSet ->	get project
		>>-	\project ->	get recentProjects
		>>- \recProj -> let isProj 			= project.projectPath <> "" && project.projectName <> ""
							isCPM  			= curSet.cpmDirectory <> ""
							projectPath 	= project.projectPath </> project.projectName +++ ".prj"
							applicationPath = curSet.cpmDirectory
						in	(if (not isCPM)  (viewInformation () [] "Where is cpm.exe ? Should be in Clean home directory...")
							(if (not isProj) (viewInformation () [] ("The location of cpm.exe currently set is: " +++ curSet.cpmDirectory )
											 -||-
											 viewInformation () [] "No Project Set...")
					   			(viewInformation () [] "Change Settings..")
							))
							>>*		[ OnAction (Action "Set cpm location")	(always 		(mbCancel findCPM <<@ ApplyLayout frameCompact))
							 		, OnAction (Action "Set Project")		(ifCond isCPM   (mbCancel NewProject <<@ ApplyLayout frameCompact))
									, OnAction (Action "Use Recent Project")(ifCond (isCPM && recProj <> []) 
																							(mbCancel useRecentProject <<@ ApplyLayout frameCompact))  
									, OnAction (Action "Project Options")		(ifCond (isCPM && recProj <> []) 
																							(projectOptionsEditor projectPath applicationPath))  
							 		]

	findCPM 
		= 					askUserForFile cpmFile isCPM 
		>>- \(path,file) ->	upd (\curSet -> {curSet & cpmDirectory  = path}) settings

	NewProject  
		= 					askUserForFile "the Clean .icl main file" isCleanIcl 
		>>- \(path,file) -> upd (\myProj -> { myProj 	& projectPath 		= path
														, projectName 		= dropExtension file
														, projectSources 	= [path]
				  							}) project
		>>|					AddStdEvironment
		>>|					get settings 
		>>- \curSet ->		get project
		>>- \myProj ->		createProject (curSet.cpmDirectory </> cpmFile) myProj.projectPath myProj.projectName
		>>|					addToRecentProjects myProj

	AddStdEvironment :: Task ()
	AddStdEvironment 
		=					get settings
		>>- \curSet ->  	fetchDirectories (curSet.cpmDirectory </> "Libraries") isCleanIclDcl
		>>- \libs ->		upd (\cs -> {cs & environmentName = "MyLibraries"
											, environmentLibs = [libs]
										}) environment @! ()
								
	useRecentProject :: Task ()
	useRecentProject 
		=				get recentProjects
		>>- \recent ->	enterChoice "Select previous project to open..." [ChooseFromGrid (\rec -> rec.projectName)] recent
		>>* 			[ OnAction (Action "Use") (hasValue (\oldProj -> set oldProj project @! () ))
						] 


// -------

findFileInProjectEnv :: String -> Task (Maybe (Bool,FilePath))
findFileInProjectEnv fileName
	=				findFileInProject fileName  
	>>- \mbFile ->	if (isNothing mbFile) 
						(findFileInEnvironment fileName)
						(return mbFile)
where
	findFileInProject :: String  -> Task (Maybe (Bool,FilePath))
	findFileInProject fileName 
		= 				get project
		>>- \myProj ->	allTasks [fetchDirectories filePath ((==) fileName) \\ filePath <- myProj.projectSources]
		>>- \dirs ->	case findInDirectories fileName dirs of
							Nothing 	-> return Nothing
							Just path	-> return (Just (False,path))
	
	findFileInEnvironment :: String -> Task (Maybe (Bool,FilePath))
	findFileInEnvironment fileName 
		= 				get environment
		>>- \curEnv ->	case findInDirectories fileName curEnv.environmentLibs of
							Nothing 	-> return Nothing
							Just path	-> return (Just (True,path))

