module test_unclosing_save_window

import iTasks
import System.OS
import StdArray
import StdFile, System.File
import iTasks.API.Extensions.Editors.Ace
import Text

Start world = startEngine
	starttask
	world

derive class iTask EditorInfo, Shortcut
	
:: EditorInfo = 
	{
		shortcuts :: [Shortcut],
		selection :: Maybe AceRange,
		position :: (Int,Int),
		theme :: String,
		readOnly :: Bool,
		prev_time :: Time
	}
	
editorInfo = 
	{
		shortcuts = [],
		position = (0,0),
		selection = Nothing,
		theme = "",
		readOnly = False,
		prev_time = {Time|hour=0,min=0,sec=0}
	}
	
:: Shortcut = No_shortcut 
			| Ctrl_slash 
			| Ctrl_backslash 
			| Ctrl_Shift_backslash 
			| Ctrl_equals 
			| Ctrl_Shift_equals 
			| Ctrl_b //
			| Ctrl_d
			| Ctrl_Shift_d
			| Ctrl_e
			| Ctrl_Shift_e
			| Ctrl_i
			| Ctrl_l
			| Ctrl_m
			| Ctrl_n
			| Ctrl_o
			| Ctrl_r
			| Ctrl_Shift_r
			| Ctrl_s
			| Ctrl_Shift_s
			| Ctrl_Alt_s
			| Ctrl_Shift_Alt_s
			| Ctrl_w
			| Ctrl_Shift_w

contents :: Shared [String]
contents = sharedStore "contents" []

filename :: String
filename = "C:\\Users\\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\research-internship\\aaa.txt"

joinWithNewline :: String String -> String
joinWithNewline a b = a+++OS_NEWLINE+++b

editor :: Task (AceOptions,AceState)
editor = (withShared editorInfo (\ei.
	(updateSharedInformation "" [] contents) ||- (updateSharedInformation "" [UpdateUsing id (\_ nsc -> nsc) aceEditor] (er ei))))
	where
	er ei = mapReadWrite ((eiAndContents2ErRead), (er2EiAndContentsWrite)) (ei >*< contents)

eiAndContents2ErRead :: (EditorInfo,[String])  -> (!AceOptions,!AceState)
eiAndContents2ErRead (ei,contents)  =
	(
		{AceOptions|
			theme = ei.EditorInfo.theme,
			mode = "mode-haskell"//"C:\\Users\\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\research-internship\\ace\\ace-master\\lib\\ace\\mode\\clean.js"
		}
		,
		{AceState|
			lines = ["harry":contents] ,
			cursor = ei.EditorInfo.position,
			selection = ei.EditorInfo.selection,
			disabled = ei.EditorInfo.readOnly
		}
	)

er2EiAndContentsWrite :: (!AceOptions,!AceState) (EditorInfo,[String]) -> Maybe (EditorInfo,([String]))
er2EiAndContentsWrite (ao,as) (ei,contents) =
	Just (
		{EditorInfo|
			shortcuts = ei.EditorInfo.shortcuts,
			selection = as.AceState.selection,
			position = as.AceState.cursor,
			theme = ao.AceOptions.theme,
			readOnly=as.AceState.disabled,
			prev_time=ei.EditorInfo.prev_time
		}, 
		["klaas":contents]
	)
	
starttask :: Task (AceOptions,AceState)
starttask = 
	editor
	>>*[	OnAction ActionSaveAs		(always 
			(contentOf >>- \content. 
			saveFileAs filename content >>| starttask))
			,
			OnAction ActionOpen (always
			(setContents filename >>| starttask))
			,
			OnAction (Action "Restart") (always
			(viewInformation "" [] "" >>| starttask))
	 	]
	 
	 	
contentOf :: Task String
contentOf = get contents >>- \c.
	return (foldr joinWithNewline "" (c))
	
setContents :: String -> Task ()
setContents iclloc
	= 							(readLinesFromFile (iclloc))
	>>- \mct -> case mct of
		Nothing = viewInformation "" [] iclloc >>| return ()
		(Just contenttxt) =	set contenttxt contents
				>>| return ()
				
remove_double_enters :: String -> String
remove_double_enters str = {c \\ c <-: str | c <> '\r'}

remove_newlines :: String -> String
remove_newlines str = {c \\ c <-: str | c <> '\n'}

saveFile :: String String -> Task ()
saveFile path content = writeToFile path (remove_double_enters content) /*>>- ReadFromFile*/ @! () 

saveFileAs :: String String -> Task ()
saveFileAs path content
		= 		updateInformation "Where to write to ?" [] path
		>>*		[	OnAction ActionSave		(hasValue (\name -> saveFile name content))
			 	,	OnAction ActionCancel	(always (return ()))
			 	]
       
readLinesFromFile :: String -> Task (Maybe [String])
readLinesFromFile path = worldIO (read path) 
where 
	read path world
	# (ok,file,world)			= fopen path FReadData world
	| not ok					= (Ok Nothing, world) 
	# (res,file)				= readAllLines file []
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot close file: " +++ path), world)
    =  (Ok (Just res), world)

	readAllLines file accu 
	# (line,file) 				= freadline file
	| line == ""				= (reverse accu,file)
	= readAllLines file [(remove_newlines line):accu]

writeToFile :: String String -> Task String
writeToFile path content = worldIO (write path content) 
where 
	write path content world
	# (ok,file,world)			= fopen path FWriteText world
	| not ok					= (Error ("Cannot open file: " +++ path), world)
	# file						= fwrites content file
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot close file: " +++ path) ,world)
	= (Ok content, world)