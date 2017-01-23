implementation module Platform

import StdPSt
import ArgEnv
import StdArray, StdEnum, StdList, StdClass, StdBool
import UtilIO
from StdSystem import dirseparator
import code from library "platform_kernel_library"
import clCCall_12, ostoolbox

PlatformDependant win mac :== win

cl_args =: getCommandLine

filter_opts [] = []
filter_opts [h:t]
	| h.[0] == '-'	= filter_opts (drop 1 t)
	= [h:filter_opts t]

get_arg n
	= get_arg n [a \\ a <-: cl_args]
where
	get_arg n [] = (False,"")
	get_arg n [h:t]
		| h == n = case t of [] -> (True,""); [h:_] -> (True,h)
		= get_arg n t

get_env n
	# e = getEnvironmentVariable n
	= case e of
		EnvironmentVariableUndefined	-> (False,"")
		EnvironmentVariable v			-> (True,v)

get_ini file section key default`
	#! (buffer,tb) = winMakeCString (createArray 256 '@') OSNewToolbox
	   (size,tb) = GetPrivateProfileString section key default` buffer 256 file tb
	   (buffer,tb) = winGetCStringAndFree buffer tb
	= buffer%(0,size-1)
where
	GetPrivateProfileString :: !String !String !String !CSTR !Int !String !*OSToolbox -> (!Int,!*OSToolbox)
	GetPrivateProfileString _ _ _ _ _ _ _ = code {
		ccall GetPrivateProfileStringA@24 "PsssIIs:I:I"
		}

initPlatformCommandLine :: !*(PSt *l) -> (![String],!*PSt *l)
initPlatformCommandLine ps
	# args	= cl_args
	# args	= [arg \\ arg <-: args]
	# args = filter_opts args
	| isEmpty args
		= ([],ps)
	# files	= tl args
	# files	= map GetLongPathName files
	= (files, ps)

installPlatformEventHandlers :: !*(PSt *l) -> *(PSt *l)
installPlatformEventHandlers ps
	= ps

openPlatformWindowMenu :: !*(PSt *l) -> *(PSt *l)
openPlatformWindowMenu ps
	= ps

//====

inifilename
	# apppath = winGetModulePath
	# s = size apppath
	=: apppath%(0,s-4) +++. "ini\0"

section =: "Paths\0"
toolkey =: "tooltemp\0"
envskey =: "envsdir\0"
prefskey =: "prefsdir\0"

TooltempDir :: String
TooltempDir =: let
	(has_arg,arg) = get_arg "-tooltemp"
	(has_env,env) = get_env "TOOLTEMP"
	ini = get_ini inifilename section toolkey (StartUpDir+++."\0")
	in if has_arg arg (if has_env env ini)

EnvsDir :: String
EnvsDir =: let
	(has_arg,arg) = get_arg "-envsdir"
	(has_env,env) = get_env "ENVSDIR"
	ini = get_ini inifilename section envskey (StartUpDir+++."\0")
	in if has_arg arg (if has_env env ini)

PrefsDir :: String
PrefsDir =: let
	(has_arg,arg) = get_arg "-prefsdir"
	(has_env,env) = get_env "PREFSDIR"
	ini = get_ini inifilename section prefskey (StartUpDir+++."\0")
	in if has_arg arg (if has_env env ini)

StartUpDir :: !String
StartUpDir =: expand_8_3_names_in_path (RemoveFileName WinGetModulePath)

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

WinGetModulePath ::  {#Char}
WinGetModulePath
	= code
	{
		.inline WinGetModulePath
			ccall WinGetModulePath "-S"
		.end
	}

