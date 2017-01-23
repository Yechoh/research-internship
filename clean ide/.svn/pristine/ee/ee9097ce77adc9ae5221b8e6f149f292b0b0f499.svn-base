implementation module PmPath

import StdClass,StdString, StdChar, StdBool, StdChar,StdInt, StdMisc,StdArray;
import StdPathname

from StdSystem import dirseparator
import PmTypes
import Platform
import UtilStrictLists

/* The name of the system directory */
SystemDir			:== "Clean System Files";

//--

IsDefPathname :: !Pathname -> Bool;
IsDefPathname name =  equal_suffix ".dcl" name;

IsImpPathname :: !Pathname -> Bool;
IsImpPathname name =  equal_suffix ".icl" name;
	
IsABCPathname :: !Pathname -> Bool;
IsABCPathname name =  equal_suffix ".abc" name;
	
IsPrjPathname :: !Pathname -> Bool;
IsPrjPathname name =  equal_suffix ".prj" name;

MakeDefPathname	:: !String -> Pathname;
MakeDefPathname name =  RemoveSuffix name  +++ ".dcl";

MakeImpPathname	:: !String -> Pathname;
MakeImpPathname name = RemoveSuffix name  +++ ".icl";
			
MakeABCPathname	:: !String -> Pathname;
MakeABCPathname name = RemoveSuffix name  +++ ".abc";
	
MakeObjPathname	:: !Processor !String -> Pathname;
MakeObjPathname processor name
	| processor == CurrentProcessor
		= RemoveSuffix name  +++ ".o";
	| processor == MC68000
		= RemoveSuffix name +++ ".obj0";
	| processor == MC68020
		= RemoveSuffix name +++ ".obj1";
	| processor == MC68020_and_68881
		= RemoveSuffix name +++ ".obj2";
		= abort ("MakeObjPathname: " +++  toString processor +++ " : No such processor ");
	
MakeProjectPathname	:: !String -> Pathname;
MakeProjectPathname name = RemoveSuffix name   +++ ".prj";

MakeExecPathname :: !String -> Pathname;
MakeExecPathname name
	= PlatformDependant
		(RemoveSuffix name+++".exe")	// Win
		(RemoveSuffix name)				// Mac

MakeSystemPathname :: !Pathname -> Pathname;
MakeSystemPathname pathname
	| equal_suffix SystemDir pathname
		= pathname
	| size pathname > 0 && pathname.[size pathname - 1] == dirseparator
		= pathname +++ SystemDir
	| otherwise
		= pathname +++ sep +++ SystemDir;
where
	sep = toString dirseparator;

MakeABCSystemPathname :: !Pathname -> Pathname
MakeABCSystemPathname abcname
	= directory_name_plus_system_dir +++ sep +++ file +++ ".abc"
where
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
			| size dir > 0 && dir.[size dir - 1] == dirseparator
				= dir +++ SystemDir;
			| otherwise
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename abcname;
		sep		= toString dirseparator;
		file	= RemovePath (RemoveSuffix abcname);
	
MakeObjSystemPathname :: !Processor !Pathname -> Pathname
MakeObjSystemPathname processor name
	| processor == CurrentProcessor
		= files_and_path ".o";
	| processor == MC68000
		= files_and_path ".obj0";
	| processor == MC68020
		= files_and_path ".obj1";
	| processor == MC68020_and_68881
		= files_and_path ".obj2";
		= abort ("MakeObjSystemPathname: " +++  toString processor +++ " : No such processor ");
where
		files_and_path extension = directory_name_plus_system_dir +++ sep +++ file+++extension
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
			| size dir > 0 && dir.[size dir - 1] == dirseparator
				= dir +++ SystemDir;
			| otherwise
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename name;
		sep		= toString dirseparator;
		file	= RemovePath (RemoveSuffix name);

MakeAssemblySystemPathname :: !Pathname -> Pathname
MakeAssemblySystemPathname abcname
	= directory_name_plus_system_dir +++ sep +++ file +++ suffix
where
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
			| size dir > 0 && dir.[size dir - 1] == dirseparator
				= dir +++ SystemDir;
			| otherwise
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename abcname;
		sep		= toString dirseparator;
		file	= RemovePath (RemoveSuffix abcname);
		suffix	= PlatformDependant
					".s"	// Win
					".a"	// Mac
	
/*
MakeABCSystemPathname :: !Pathname !Files -> (!Pathname,!Files);
MakeABCSystemPathname abcname files
	= (directory_name_plus_system_dir +++ sep +++ file +++ ".abc",files);
where
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename abcname;
		sep		= toString dirseparator;
		file	= RemovePath (RemoveSuffix abcname);
	
MakeObjSystemPathname :: !Processor !Pathname !Files -> (!Pathname,!Files);
MakeObjSystemPathname processor name files
	| processor == CurrentProcessor
		= files_and_path ".o";
	| processor == MC68000
		= files_and_path ".obj0";
	| processor == MC68020
		= files_and_path ".obj1";
	| processor == MC68020_and_68881
		= files_and_path ".obj2";
		= abort ("MakeObjSystemPathname: " +++  toString processor +++ " : No such processor ");
where
		files_and_path extension = (directory_name_plus_system_dir +++ sep +++ file+++extension,files);
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename name;
		sep		= toString dirseparator;
		file	= RemovePath (RemoveSuffix name);
*/	

GetModuleName :: !Pathname -> Modulename;
GetModuleName name =  RemoveSuffix (RemovePath name);

//==

symPath :: !Pathname !Pathname !Pathname -> Pathname
symPath ap pp l
	| size ap >= size pp		// generate shortest symbolic path...
		#	l = replace_prefix_path ap "{Application}" l
			l = replace_prefix_path pp "{Project}" l
		= l
	| otherwise
		#	l = replace_prefix_path pp "{Project}" l
			l = replace_prefix_path ap "{Application}" l
		= l

fulPath :: !Pathname !Pathname !Pathname -> Pathname
fulPath ap pp l
	#	l = replace_prefix_path "{Application}" ap l
		l = replace_prefix_path "{Project}" pp l
	= l

symPaths :: !Pathname !Pathname !(List Pathname) -> List Pathname
symPaths ap pp l = Map (symPath ap pp) l

fulPaths :: !Pathname !Pathname !(List Pathname) -> List Pathname
fulPaths ap pp l = Map (fulPath ap pp) l

symAppPath :: !Pathname !Pathname -> Pathname
symAppPath ap p
	= replace_prefix_path ap "{Application}" p

fulAppPath :: !Pathname !Pathname -> Pathname
fulAppPath ap p
	= replace_prefix_path "{Application}" ap p

symAppPaths :: !Pathname !(List Pathname) -> List Pathname
symAppPaths ap l = Map (symAppPath ap) l

fulAppPaths :: !Pathname !(List Pathname) -> List Pathname
fulAppPaths ap l = Map (fulAppPath ap) l
