module webIDE

import iTasks
/*
import iTasks.API.Extensions.Admin.WorkflowAdmin
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor.Builtin*/
import iTasks.API.Extensions.Editors.Ace
import StdFile, System.File
import System.Directory
import System.FilePath
import _SystemArray/*
import Data.Error
import qualified Data.Map as DM
import PmDriver*/
import Text
import System.OS
/*
import PmProject
*/
import createAndRunExec
import directoryBrowsing
/*
import CpmLogic2
*/
// -------------

import qualified System.Process as SP

import qualified Data.Map as DM
:: Settings = 	{ dirCpm 	:: FilePath
				, dirClean	:: FilePath
				, dirClean2 :: FilePath
				, dirIDEEnvs :: FilePath
				, dirCmd :: FilePath
				}
derive class iTask Settings

settings = sharedStore "settings" 	{ dirCpm = "C:\\Users\\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\clean-bundle-itasks\\cpm.exe"
									, dirClean = "C:\Users\Martin\Documents\clean-bundle-itasks-windows-x86-latest\clean-bundle-itasks"
									, dirClean2 = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223"
									, dirIDEEnvs = "C:\\Users\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\clean-bundle-itasks\\Config\\IDEEnvs"
									, dirCmd = "C:\\WINDOWS\\WinSxS\\wow64_microsoft-windows-commandprompt_31bf3856ad364e35_10.0.10586.0_none_21e70967f9147e9b\\cmd.exe"
									}
updSettings 
	= 		updateSharedInformation "Current Setting: " [] settings


// utility functions

isCleanFile file = isMember (takeExtension file) ["icl", "dcl", "prj", "abc", "sapl"]
isFile str file = (takeExtension file) == str

selectIcon pwd _ 	 = Nothing	// don't know where to store icons yet
/*selectIcon pwd "icl" = Just (pwd </> "WebPublic" </> "Clean.icl.ico")
selectIcon pwd "dcl" = Just (pwd </> "WebPublic" </> "Clean.dcl.ico")
selectIcon pwd "prj" = Just (pwd </> "WebPublic" </> "Clean.prj.ico")
selectIcon pwd "abc" = Just (pwd </> "WebPublic" </> "Clean.abc.ico")
selectIcon pwd _ 	 = Nothing*/

// missing operator hack for avoiding continues 

(>>|-) infixl 1 :: (Task a) (Task b) -> Task b | iTask a & iTask b
(>>|-) ma mb = ma >>- \_ -> mb

// -------------

Start :: *World -> *World
Start world = startEngine
	//(editor "" "")
	//openLog
	//(callProcess "Create project" [] "C:/Users/Martin/Documents/clean-bundle-itasks-windows-x86-latest/clean-bundle-itasks/cpm.exe"["project","C:\Users\Martin\Documents\clean-bundle-itasks-windows-x86-latest\clean-bundle-itasks\Test.prj","create"] Nothing )
	(askPath >>- \(path,name,cur)-> ((setContent path name) >>|- (setProject path name cur) >>|- (editFile path name cur)))
	/*((accWorld ('SP'.callProcess 
	(replaceExtension "C:/Users/Martin/Documents/clean-bundle-itasks-windows-x86-latest/clean-bundle-itasks/cpm.exe" "bat") 
	["C:/Users/Martin/Documents/clean-bundle-itasks-windows-x86-latest/clean-bundle-itasks/Test.prj","C:/Users\Martin/Documents/clean-bundle-itasks-windows-x86-latest/clean-bundle-itasks/Config/IDEEnvs"] Nothing))
	>>- \err. viewInformation "" [] err)*/
	//(readFromFile "jemoeder")
	//ideDashboard
	//showUnresolvedImports
	/*(
	viewInformation "" [] "ok1?" >>| 
	getPwdName >>= \pwd.
	get settings >>= \settings2.
	viewInformation "" [] (pwd+++settings2.dirClean2) >>|
	viewInformation "" [] "ok3?"
	)*/
	//(askImports "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship\\eigen filesystem\\EditedwebIDE.icl")
	world


openLog :: Task String
openLog =
 get settings >>- 
 \settings. viewInformation "" [] (replaceFileName settings.dirCpm "log.txt") -|| 
  (readFromFile (replaceFileName settings.dirCpm "log.txt") >>- \x. viewInformation "" [] x)

// ------------- to do:

selectFromTreeWithOption :: String -> Task String
selectFromTreeWithOption pwd = selectFromTree pwd isCleanFile [OnAction (Action "Change paths")	(always ((updateSharedInformation "paths" [] settings)  >>| selectFromTreeWithOption pwd))]

askPath :: Task (String,String)
askPath
	=							getPwdName
	>>- \pwd ->					selectFromTreeWithOption pwd
	>>* [OnAction ActionContinue ((ifValue (isFile "icl") (\path -> return (takeDirectory path, dropDirectory path))))]

setContent :: String String -> Task ()
setContent path name
	= 							readFromFile (path </> name)
	>>- \(Just contenttxt) ->		set contenttxt content
								>>|- return ()
	
setProject :: String String -> Task ()
setProject path name
	=							readFromFile (toproj (path </> name))
	>>- \mprojtxt ->			get settings
	>>- \settings ->			(case mprojtxt of
		Nothing ->					(accWorld ('SP'.callProcess settings.dirCpm ["project",toproj name,"create"] Nothing) 
									>>|- return ())
		(Just projtxt) ->			return ()
								)
/*
treeEdit :: Task ()//(({#Char},String),())
treeEdit 
	=(							getPwdName
 	>>- \pwd ->					selectFromTreeWithOption pwd
 	>>* [ OnAction ActionContinue ((ifValue (isFile "icl") 
 	(\path ->					readFromFile path
	 \(Just contenttxt) -> 		readFromFile (toproj path)
	>>- \mprojtxt -> 			get settings		 
	>>- \settings ->			(case mprojtxt of
	Nothing	->					(callProcess "Create project" [] settings.dirCpm ["project",(pwd </> "Temp" </> (toproj (dropDirectory path))),"create"] Nothing >>| return ())
	//appWorld (createProject (pwd </> "Temp" </> (dropDirectory path)))
	(Just projtxt) -> 			writeToFile (pwd </> "Temp" </> (toproj (dropDirectory path))) projtxt >>| return ())
	 /*if (isJust mprojtxt) (writeToFile (pwd </> "Temp" </> (toproj (dropDirectory path))) (fromJust mprojtxt) >>| return ()) (appWorld (createProject (pwd </> "Temp" </> (dropDirectory path))))*/
	>>| 						set contenttxt content  
	>>|							editFile (takeDirectory path) (dropDirectory path) pwd)))])
*//*
treeEdit :: Task ()//(({#Char},String),())
treeEdit 
	=							getPwdName
 	>>- \pwd ->					selectFromTree pwd isCleanFile []
 	>>* [   OnAction  ActionContinue   			(ifValue (isFile "icl") (\path -> openAndEdit path pwd))
	 	,	OnAction (Action "Change paths")	(always ((updateSharedInformation "paths" [] settings) -&&- (viewInformation "" [] "The map research-internship is expected to be in the Itasks-SDK map in the libraries map in the clean directory") >>| treeEdit))
	    ]
openAndEdit :: String String -> Task ()
openAndEdit path pwd =			
								readFromFile path
	>>- \(Just contenttxt) -> 	readFromFile (toproj path) 
	>>- \mprojtxt ->			(case mprojtxt of
	Nothing	->					appWorld (createProject (pwd </> "Temp" </> (dropDirectory path)))
	(Just projtxt) -> 			writeToFile (pwd </> "Temp" </> (toproj (dropDirectory path))) projtxt >>| return ())
	>>| 						set contenttxt content  
	>>|							editFile (takeDirectory path) (dropDirectory path) pwd
*/
/*
//# (exists,world)  = fileExists mainmodule world
createTempFiles :: String String -> Task (String,String)
createTempFiles path name =
	accWorld (getCurrentDirectory) >>=
	\cur. 
	(createFile (cur <\> "Temp" <\> name) "" -&&-
	 createFile (cur <\> "Temp" <\> replaceExtension name "prj")
*/

editor :: String String -> Task {#Char}
editor path name =
	updateSharedInformation (path </> name) [UpdateUsing id (\_ nsc -> nsc) aceTextArea] content

//updaters
//updateEverySecond :: 

//end updaters

cpmtask :: String -> Task ()
cpmtask iclloc = 
			get currentTime >>=
			\now.  waitForTime2 {Time| now & sec=now.Time.sec+1} >>- \_ ->
			(saveFile iclloc content) >>- \_ ->
			get settings >>- \settings ->
			appWorld (\w. snd ('SP'.runProcess settings.dirCmd ["/c",settings.dirCpm,toproj iclloc,"--envs="+++settings.dirIDEEnvs,"1>templog.txt","2>errlog.txt"] Nothing w))
			>>|- readFromFile "templog.txt"
			>>- \(Just errors). 
			set (errors) errorstate
			>>- \_ -> cpmtask iclloc
	where
	saveFile path content 
		= get content >>= \c. writeToFile path c @! ()
	prjloc = replaceExtension iclloc "prj"
	errorloc dirCpm = (takeDirectory dirCpm)  </> "Temp" </> "errors"

//(ifValue (\(a,now1).get currentTime >>= \now2. (now2.sec > now1.sec)))

waitForTime2 :: !Time -> Task Time
waitForTime2 time =
	watch currentTime >>* [OnValue (ifValue (\now -> time < now) return)]

Errors2Imports :: String -> String
Errors2Imports errors 
	# lines = split OS_NEWLINE errors
	# importlines = filter (\line. endsWith "imported" line) lines
	# importlinessplitted = map (split " ") importlines
	# dclnames = map (\x. hd (tl (tl x))) importlinessplitted
	=  join "\n" dclnames

import iTasks.UI.Editor.Builtin
import Text, Text.HTML, Data.List

showUnresolvedImports :: Task String
showUnresolvedImports = 
	viewSharedInformation "Unresolved imports"  /*[ViewUsing (\s -> SpanTag [] (intersperse (BrTag []) (map Text (split "\n" s)))) (htmlView 'DM'.newMap)] */[ViewUsing Errors2Imports (textArea 'DM'.newMap)] errorstate

addPath2Project :: String String String -> String
addPath2Project path cleandir projtxt
	# short_path = dropFileName (subString (textSize cleandir) (textSize path) path)
	# newpath = "\t\tPath:\t{Application}" +++ short_path
	# splitted_projtxt = split ("Path:\t{Project}"+++OS_NEWLINE) projtxt
	# paths = hd (tl splitted_projtxt)
	# newpaths = newpath +++ OS_NEWLINE +++ paths
	# newprojtxt = join ("Path:\t{Project}"+++OS_NEWLINE) [(hd splitted_projtxt),newpaths]
	= newprojtxt
	//= join "-" splitted_projtxt
	//= path
	//= projtxt

showMapSelector :: String -> Task ()
showMapSelector iclloc = 
	get settings
	>>= \settings. selectFromTree settings.dirClean2 (isFile "dcl") []
	>>= \path. readFromFile projloc 
	//>>- \projtxt. (viewInformation "" [ViewUsing id (textArea 'DM'.newMap)] (addPath2Project path settings.dirClean projtxt )) >>| return () 
	>>- \(Just projtxt). saveFile projloc (addPath2Project path settings.dirClean projtxt)
	>>- \_ -> callProcess "Compile project" [] settings.dirCpm [projloc, "--envs="+++settings.dirIDEEnvs] Nothing
	//appWorld (compile projloc) 
	>>- \_ -> readFromFile errorloc
	>>- \(Just errors). 
	set (errors) errorstate
	>>- \_ -> showMapSelector iclloc
	where
		projloc = toproj iclloc
		errorloc = replaceFileName iclloc "errors"

askImports :: String (Task ()) -> Task ()
askImports iclloc uppertask = 
	showUnresolvedImports
	||-
	showMapSelector iclloc
	>>* [   OnAction  ActionQuit    	(always (uppertask))
		]



editFile :: String String -> Task () //(({#Char},String),())
editFile path name =
	 		(editor path name 
	 		-&&-
	 		viewSharedInformation "errors" [ViewUsing id (textArea 'DM'.newMap)] errorstate) 
	 		-&&-
	 		(cpmtask (name))
	 >^*	[	OnAction  ActionSave    	(always (get content >>= \content. saveFile (path </> (name)) content))
	 		,	OnAction  ActionSaveAs		(always (get content >>= \content. saveFileAs (path </> name)content )) 
	 		]
	 		++ 
	 		if (takeExtension name <> "icl") []
	 		[	OnAction (Action "Run")		(always (runExec (path </> dropExtension name +++ ".exe") 8080))	
	 		]
	 >>*	[   OnAction  ActionQuit    	(always (return ()))
	 		,	OnAction (Action "Import")	(always (askImports (name) (editFile path name)))

		    ]
where	
	saveFileAs path content
		= 		updateInformation "Where to write to ?" [] path
		>>*		[	OnAction ActionSave		(hasValue (\name -> saveFile name content))
			 	,	OnAction ActionCancel	(always (return ()))
			 	]

toproj :: String -> String
toproj path = replaceExtension path "prj"

remove_double_enters :: String -> String
remove_double_enters str = {c \\ c <-: str | c <> '\r'}

saveFile :: String String -> Task ()
saveFile path content = writeToFile path (remove_double_enters content) /*>>- ReadFromFile*/ @! () 



