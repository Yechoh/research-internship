implementation module UtilIO

/*
 modify to use 'clCCall_12' functions instead of redefining them here...
*/
import StdArray, StdBool, StdClass, StdFile, StdList
import UtilDate

import StdSystem
import code from library "util_io_kernel_lib"

//dirseparator	:==	'\\'				// OS separator between folder- and filenames in a pathname

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
	
::	OSToolbox
	:==	Int

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
	#! s_short_path
		= GetShortPathName_ long_path "\0" 0;
	#! short_path
		= createArray s_short_path '\0';
	#! result
		= GetShortPathName_ long_path short_path s_short_path;
	= (result <> 0,short_path);
where
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

