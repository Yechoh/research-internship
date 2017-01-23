implementation module UtilIO

/*
 modify to use 'clCCall_12' functions instead of redefining them here...
*/
import StdArray, StdBool, StdClass, StdFile, StdList
import UtilDate

import StdSystem
import code from library "util_io_kernel_lib"
import code from library "util_io_shell_lib"

//--

CcRqSHELLDEFAULT	:== 1476

osIgnoreCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
osIgnoreCallback _ tb 
	= (return0Cci,tb)

osShellDefault :: !{#Char} !*OSToolbox -> (!Int,!*OSToolbox)
osShellDefault file tb
//	# tb = winInitialiseTooltips tb
	# (cstr,tb) = winMakeCString file tb
	# (ret,tb) = (issueCleanRequest2 osIgnoreCallback (Rq2Cci CcRqSHELLDEFAULT 0 cstr) tb)
	# tb = winReleaseCString cstr tb
	= (ret.p1,tb)


//---
SW_SHOWNORMAL	:== 1

ShellDefault :: !{#Char} !(PSt .l) -> (!Int,!(PSt .l))
ShellDefault file ps
	= accPIO (accIOToolbox (osShellDefault file)) ps
//	= accPIO (accIOToolbox (ShellExecute 0 0 file 0 0 SW_SHOWNORMAL)) ps

ShellExecute :: !Int !Int !{#Char} !Int !Int !Int !*OSToolbox -> (!Int,!*OSToolbox)
ShellExecute _ _ _ _ _ _ _ = code {
		ccall ShellExecuteA@24 "IIsIII:I:I"
	}

//---

import StdPathname, Directory

FReadOnly :: !{#Char} !*env -> (!Bool, !*env) | FileSystem env
FReadOnly path files
	# ((ok,dir),files)	= pd_StringToPath path files
	| not ok
		= (False,files)
	# ((err,fi),files)	= getFileInfo dir files
	| err <> NoDirError
		= (False,files)
	= (fi.pi_fileInfo.isReadOnly,files)

/* old version...
FReadOnly path files
	# dir				= RemoveFilename path
	# fnm				= RemovePath path
	# ((ok,dir),files)	= pd_StringToPath dir files
	| not ok
		= (False,files)
	# ((err,dct),files)	= getDirectoryContents dir files
	| err <> NoDirError
		= (False,files)
//	# dct				= map (\{fileInfo}->fileInfo.pi_fileInfo) dct
	# dct				= filter (\{fileName}->fileName==fnm) dct
	| isEmpty dct
		= (False,files)
	= ((hd dct).fileInfo.pi_fileInfo.isReadOnly,files)
*/

FFileSize :: !{#Char} !*env -> (!(!Bool,!Int), !*env) | FileSystem env
FFileSize path files
	# ((ok,dir),files)	= pd_StringToPath path files
	| not ok
		= ((False,0),files)
	# ((err,fi),files)	= getFileInfo dir files
	| err <> NoDirError
		= ((False,0),files)
	= ((True,toInt fi.pi_fileInfo.fileSize),files)


//--
	
//::	OSToolbox
//	:==	Int

WinLaunchApp ::  !{#Char} !Bool !*OSToolbox -> ( !Bool, !*OSToolbox)
WinLaunchApp _ _ _
	= code
	{
		.inline WinLaunchApp
			ccall WinLaunchApp "SII-II"
		.end
	}

WinLaunchApp2 :: !{#Char} !{#Char} !Bool !*OSToolbox -> ( !Bool, !*OSToolbox)
WinLaunchApp2 _ _ _ _
	= code
	{
		.inline WinLaunchApp2
			ccall WinLaunchApp2 "SSII-II"
		.end
	}

WinGetModulePath ::  {#Char}
WinGetModulePath
	= code
	{
		.inline WinGetModulePath
			ccall WinGetModulePath "-S"
		.end
	}

WinFileModifiedDate ::  !{#Char} -> ( !Bool, !Int, !Int, !Int, !Int, !Int, !Int)
WinFileModifiedDate _
	= code
	{
		.inline WinFileModifiedDate
			ccall WinFileModifiedDate "S-IIIIIII"
		.end
	}

WinFileExists ::  !{#Char} ->  Bool
WinFileExists _
	= code
	{
		.inline WinFileExists
			ccall WinFileExists "S-I"
		.end
	}

//--

LaunchApplication :: !{#Char} !{#Char} !Bool !Files -> ( !Bool, !Files)
LaunchApplication execpath homepath console files
	# (ok,_) = WinLaunchApp2 execpath homepath console 42
	= (ok,files)

LaunchApplication` :: !{#Char} !Bool !Files -> ( !Bool, !Files)
LaunchApplication` execpath  console files
	# (ok,_) = WinLaunchApp execpath console 42
	= (ok,files)

/*	Returns True if the file name exists.
*/

FExists	:: !String !Files -> (!Bool, !Files)
FExists name files =  (WinFileExists name, files)


/*	Returns the last modification date of the indicated file.
*/
FModified :: !String !Files -> (!DATE, !Files);
FModified name files =  ( daterec, files);
where
  (exist, year, month, day, hour, minute, second) 
      =  WinFileModifiedDate name;
	daterec 
	  =  { exists=exist, yy=year, mm=month, dd=day,
                         h=hour, m=minute, s=second };


/*	Returns directory in which the indicated application resides.
*/

FStartUpDir :: !String !Files -> (!String, !Files);
FStartUpDir _ files = (expand_8_3_names_in_path name, files);
where 
  name  =  RemoveFileName WinGetModulePath;

GetFullApplicationPath :: !*Files -> ({#Char}, *Files);
GetFullApplicationPath files
	=	FStartUpDir "" files;


RemoveFileName :: !String -> String;
RemoveFileName path
	| found	= (path % (0, dec position));
			= path;
where 
	(found,position)	= LastColon path last;
	last				= dec (size path);
		
LastColon :: !String !Int -> (!Bool, !Int);
LastColon s i
	| i <= 0
		= (False,0);
	| dirseparator==s.[i]
	 	= (True, i);
		= LastColon s (dec i);

//-- expand_8_3_names_in_path

FindFirstFile :: !String -> (!Int,!String);
FindFirstFile file_name
	# find_data = createArray 318 '\0';
	# handle = FindFirstFile_ file_name find_data;
	= (handle,find_data);

FindFirstFile_ :: !String !String -> Int;
FindFirstFile_ file_name find_data
	= code {
		ccall FindFirstFileA@8 "Pss:I"
	}

FindClose :: !Int -> Int;
FindClose handle = code {
		ccall FindClose@4 "PI:I"
	}

find_null_char_in_string :: !Int !String -> Int;
find_null_char_in_string i s
	| i<size s && s.[i]<>'\0'
		= find_null_char_in_string (i+1) s;
		= i;

find_data_file_name find_data
	# i = find_null_char_in_string 44 find_data;
	= find_data % (44,i-1);

find_first_file_and_close :: !String -> (!Bool,!String);
find_first_file_and_close file_name
	# (handle,find_data) = FindFirstFile file_name;
	| handle <> (-1)
		# r = FindClose handle;
		| r==r
			= (True,find_data);
			= (False,find_data);
		= (False,"");

find_last_backslash_in_string i s
	| i<0
		= (False,-1);
	| s.[i]=='\\'
		= (True,i);
		= find_last_backslash_in_string (i-1) s;

expand_8_3_names_in_path :: !{#Char} -> {#Char};
expand_8_3_names_in_path path_and_file_name
	# (found_backslash,back_slash_index) = find_last_backslash_in_string (size path_and_file_name-1) path_and_file_name;
	| not found_backslash
		= path_and_file_name;
	# path = expand_8_3_names_in_path (path_and_file_name % (0,back_slash_index-1));
	# file_name = path_and_file_name % (back_slash_index+1,size path_and_file_name-1);
	# path_and_file_name = path+++"\\"+++file_name;
	# (ok,find_data) = find_first_file_and_close (path_and_file_name+++"\0");
	| ok
		= path+++"\\"+++find_data_file_name find_data;
		= path_and_file_name;

//--

GetLongPathName :: !String -> String;
GetLongPathName short_path = expand_8_3_names_in_path short_path;
// of analoog aan GetShortPathName kernelfunctie aanroepen...

GetShortPathName :: !String -> (!Bool,!String);
GetShortPathName long_path
	#! long_path = if null_terminated long_path (long_path+++."\0")
	#! (result,short_path) = Helper long_path
	#! short_path = if null_terminated short_path (short_path%(0,size short_path - 2))
	= (result <> 0,short_path);
where
	lsize = size long_path
	null_terminated = long_path.[lsize-1] == '\0'
	
	Helper long_path
		#! s_short_path
			= GetShortPathName_ long_path "\0" 0;
		#! short_path
			= createArray s_short_path '\0';
		#! result
			= GetShortPathName_ long_path short_path s_short_path;
		= (result,short_path)

	GetShortPathName_ :: !String !String !Int -> !Int;
	GetShortPathName_ long_path short_path s_short_path
		= code {
			ccall GetShortPathNameA@12 "PssI:I"
			}

GetCurrentDirectory :: (!Bool,!String)
GetCurrentDirectory
	#! buff_size	= GetCurrentDirectory_ 0 "\0"
	#! buffer		= createArray buff_size '\0'
	#! result		= GetCurrentDirectory_ buff_size buffer
	= (result <> 0, buffer)
where
	GetCurrentDirectory_ :: !Int !String -> !Int
	GetCurrentDirectory_ buffer_size buffer
		= code {
			ccall GetCurrentDirectoryA@8 "PIs:I"
			}

//--
/*
import code from library "kernel_library"

Start = GetModuleFileName

MAX_PATH :== 260

GetModuleFileName :: (!Bool,!String)
GetModuleFileName
	#! buf = createArray (MAX_PATH+1) '\0'
	#! res = GetModuleFileName_ 0 buf MAX_PATH
	= (res <> 0,buf)
where
	GetModuleFileName_ :: !Int !String !Int -> !Int
	GetModuleFileName_ handle buffer buf_length
		= code {
			ccall GetModuleFileNameA@12 "PIsI:I"
			}
*/

//====

import	StdTuple, clCCall_12, clCrossCall_12
import	StdPSt, StdMaybe, iostate
from	deviceevents	import :: SchedulerEvent
from	osfileselect	import osInitialiseFileSelectors
from	scheduler		import handleOneEventForDevices
from	commondef		import fatalError

CcRqALTDIRECTORYDIALOG	:== 1475

selectDirectory` :: !(PSt .l) -> (!Maybe String,!(PSt .l))
selectDirectory` env
//	= selectDirectory Nothing env
	# initial = global.[0]
	# (result,env) = selectDirectory initial env
	# (result,_) = case result of
					Nothing -> (result,global)
					(Just _) -> update_maybe_string result global
	= (result,env)
where
	selectDirectory :: !(Maybe String) !(PSt .l) -> (!Maybe String,!PSt .l)
	selectDirectory initial pState
		# (tb,pState)			= accPIO getIOToolbox pState
		# tb					= osInitialiseFileSelectors tb
		# (ok,name,pState,tb)	= osSelectdirectory handleOSEvent pState initial tb
		# pState				= appPIO (setIOToolbox tb) pState
		= (if ok (Just name) Nothing,pState)

	//	handleOSEvent turns handleOneEventForDevices into the form required by osSelect(in/out)putfile.
	handleOSEvent :: !OSEvent !*(PSt .l) -> *PSt .l
	handleOSEvent osEvent pState
		= thd3 (handleOneEventForDevices (ScheduleOSEvent osEvent []) pState)

	osSelectdirectory :: !(OSEvent->.s->.s) !.s !(Maybe String) !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
	osSelectdirectory handleOSEvent state initial tb
		# (initialptr,  tb)	= case initial of
								Just initial	-> winMakeCString initial   tb
								Nothing			-> (0,tb)
		# (rcci,state,tb)	= issueCleanRequest (callback handleOSEvent) (Rq1Cci CcRqALTDIRECTORYDIALOG initialptr) state tb
		# tb				= case initialptr of
								0	-> tb
								_	-> winReleaseCString initialptr   tb
		# (ok,name,tb)		= getinputfilename rcci tb
		= (ok,name,state,tb)
	where
		getinputfilename :: !CrossCallInfo !*OSToolbox -> (!Bool,!String,!*OSToolbox)
		getinputfilename {ccMsg=CcRETURN2,p1=ok,p2=ptr} tb
			| ok==0
				= (False,"",tb)
			| otherwise
				# (pathname,tb)	= winGetCStringAndFree ptr tb
				= (True,pathname,tb)
		getinputfilename {ccMsg=CcWASQUIT} tb
			= (False,"",tb)
		getinputfilename {ccMsg} _
			= osfileselectFatalError "osSelectdirectory" ("unexpected ccMsg field of return CrossCallInfo ("+++toString ccMsg+++")")

	//	callback lifts a function::(OSEvent -> .s -> .s) to
	//        a crosscallfunction::(CrossCallInfo -> .s -> *OSToolbox -> (CrossCallInfo,.s,*OSToolbox))
	callback :: !(OSEvent->.s->.s) !CrossCallInfo !.s !*OSToolbox -> (!CrossCallInfo,!.s,!*OSToolbox)
	callback handleOSEvent cci state tb = (return0Cci,handleOSEvent cci state,tb)

	osfileselectFatalError :: String String -> .x
	osfileselectFatalError function error
		= fatalError function "osfileselect" error

//== UNSAFE HACK...

import StdArray

global :: {Maybe String}
global =: {Just ""}

//update_maybe_string :: !(Maybe String) !*{(Maybe String)} -> (!(Maybe String),!*{(Maybe String)})
update_maybe_string :: !(Maybe String) !{(Maybe String)} -> (!(Maybe String),!{(Maybe String)})
update_maybe_string ms ar
//	= (ms,{ar & [0] = ms})
	= code {
		push_a 0
		pushI 0
		push_a 2
		update_a 2 3
		update_a 1 2
		updatepop_a 0 1
		update _ 1 0
		push_a 1
		update_a 1 2
		updatepop_a 0 1
	}
