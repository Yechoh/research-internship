implementation module PmCleanSystem

/* OS dependent module for powermac */
/* Interface module for calling the CLEAN compiler, code generator and linker */

//import StdEnv
import StdArray, StdBool, StdChar, StdFunc, StdInt, StdList
import StdSystem, StdPStClass
import Directory

import PmCompilerOptions, UtilStrictLists, PmPath, PmProject
import UtilNewlinesFile
import WriteOptionsFile

from PmParse import IsTypeSpec, IsImportError13, IsImportError20
from linkargs import ReadLinkErrors,WriteLinkOpts,LinkInfo`,LPathname

import xcoff_linker

import ostoolbox
from files import LaunchApplication
import memory,appleevents

KAEQueueReply :== 2

//import StdDebug,dodebug
import dodebug
trace_n _ f :== f

fopena :== fopen
fopenb :== fopen
/*
fopena :: {#.Char} .Int *a -> *(Bool,.File,*a) | FileSystem a;
fopena s i f
	= fopen s i f
fopenb :: {#.Char} .Int *a -> *(Bool,.File,*a) | FileSystem a;
fopenb s i f
	= fopen s i f
*/
//ifWindows w o :== o

// For testing update speed...
send_command_to_clean_compiler_cc a b c
	:== send_command_to_clean_compiler a b c
//	= (0,1,"")
send_command_to_clean_compiler_cg a b c
	:== send_command_to_clean_compiler a b c
//	= (0,1,"")
send_command_to_clean_compiler_ca a b c
	:== send_command_to_clean_compiler a b c
//	= (0,0,"")

standardStaticLibraries :: !LinkMethod -> List String
standardStaticLibraries method
	= case method of
		LM_Static	-> ("library0" :! "library1" :! "library2" :! Nil)
		LM_Eager	-> ("library0" :! "library1" :! "library2" :! Nil)//("kernel_library" :! "StaticClientChannel_library" :! Nil)
		LM_Dynamic	-> ("library0" :! "library1" :! "library2" :! Nil)//("kernel_library" :! "ClientChannel_library" :! Nil)

standardObjectFiles :: !Bool !Bool -> List String
standardObjectFiles stack_traces profiling
	| stack_traces
		= ("_startupTrace.o" :! "_system.o" :! "_library.o" :! Nil)
	| profiling
		= ("_startupProfile.o" :! "_system.o" :! "_library.o" :! Nil)
		= ("_startup.o" :! "_system.o" :! "_library.o" :! Nil)

//-- interface to static libraries...

getLibs :: ![String] !*Files -> (!(![String],![String]),!*Files)
getLibs [] files = (([],[]),files)
getLibs [lib:libs] files
	# (errs,slibs,files)		= getLib lib files
	# ((errs`,slibs`),files)	= getLibs libs files
	= ((errs++errs`,slibs++slibs`),files)
getLib lib files
	= ([],[],files)
/* nog niet op mac
	# (errs,slibs,files)	=  OpenArchive lib files
	# slibs					= map RemoveSuffix slibs
	= (errs,slibs,files)
*/

//--

//--

::	CodeGenerateAsmOrCode	= AsmGeneration	| CodeGeneration

instance == CodeGenerateAsmOrCode
where
	(==) :: CodeGenerateAsmOrCode CodeGenerateAsmOrCode -> Bool
	(==) AsmGeneration AsmGeneration
		=	True
	(==) CodeGeneration CodeGeneration
		=	True
	(==) _ _
		=	False

::	CompileOrCheckSyntax	= SyntaxCheck | Compilation

instance == CompileOrCheckSyntax
where
	(==) :: CompileOrCheckSyntax CompileOrCheckSyntax -> Bool
	(==) SyntaxCheck SyntaxCheck
		=	True
	(==) Compilation Compilation
		=	True
	(==) _ _
		=	False

::	CompileClearCache	= ClearCache | Don`tClearCache

instance == CompileClearCache
where
	(==) :: CompileClearCache CompileClearCache -> Bool
	(==) ClearCache ClearCache
		=	True
	(==) Don`tClearCache Don`tClearCache
		=	True
	(==) _ _
		=	False


::	CompilerMsg
	= 	CompilerOK
	| 	SyntaxError
	| 	Patherror Pathname

instance == CompilerMsg
where
	(==) CompilerOK CompilerOK = True
	(==) SyntaxError SyntaxError = True
	(==) (Patherror _) (Patherror _) = True
	(==) _ _ = False
	
::	WindowFun env :== !([String]) -> env -> env

out_file_path :: String Int -> String
out_file_path startupdir slot
	=	file_path startupdir "out" slot

errors_file_path :: String Int -> String
errors_file_path startupdir slot
	=	file_path startupdir "errors" slot

file_path :: String String Int -> String
file_path startupdir base_name slot
	| slot==0
		=	startupdir +++ toString dirseparator +++ base_name
		=	startupdir +++ toString dirseparator +++ base_name+++toString slot

/* Compiles the given file:
*/	

Compile :: !String !Bool !(WindowFun *env) !(WindowFun *env) !CompileOrCheckSyntax !Pathname !(List Pathname) !Bool !Bool !Bool
				!CompilerOptions !Pathname !*env -> (!*env,!Pathname,!CompilerMsg) | FileEnv env
Compile cocl` write_module_times errwin typewin compileOrCheckSyntax path paths projectMemoryProfiling
					projectTimeProfiling projectEagerOrDynamic
					co=:{CompilerOptions | listTypes} startupdir ps
//	# (cocl_ok,cocl)				= mangleCompiler cocl` startupdir	// platform dependant mangling...
	| not cocl_ok
		# ps					= errwin [cocl] ps
		= (ps,"",SyntaxError)

//	# signature = CleanCompilerSignature	// XOXOXOX
//	# name		= CleanCompilerName			// XOXOXOX
	# (error_code,error_n,ss)	= trace_n command send_command_to_clean_compiler_cc signature name command Wait
	| error_code <> 0 =
		( errwin (	[ "Unable to run compiler: "+++cocl
					+++ "; "+++ toString error_code
					+++ "; "+++ toString error_n
					+++ "; "+++ ss
					]) ps
		, ""
		,SyntaxError
		)
	#	((type_text_not_empty,type_text),ps)
						= accFiles (ReadTypesInfo (listTypes<>NoTypes) out_file_name) ps
		((errors,errors_and_messages_not_empty,errors_and_messages),ps)
						= accFiles (ReadErrorsAndWarnings errors_file_name) ps
//		(abcpath,ps)	= accFiles (MakeABCSystemPathname path) ps
		abcpath			= MakeABCSystemPathname path
		ps				= case errors_and_messages_not_empty of
							True	-> trace_n "errwin" errwin (StrictListToList errors_and_messages) ps
							False	-> trace_n "ok" ps
		ps				= case type_text_not_empty of
							True	-> typewin (StrictListToList type_text) ps
							False	-> ps
     = (ps,abcpath,if (error_n==1) CompilerOK errors)
where
	(cocl_ok,cocl,name,signature)	= mangleCompiler cocl` startupdir	// platform dependant mangling...

	clearCache = Don`tClearCache	// needs to be arg to compile command???
	write_module_times_string = if write_module_times " -wmt " " "
	command
		= cocl +++ clear_cache_option +++ write_module_times_string
			+++ MakeCompilerOptionsString
					compileOrCheckSyntax
					projectMemoryProfiling
					projectTimeProfiling
					projectEagerOrDynamic
					co
        	+++ " -sl"
        	+++ " -P " +++ quoted_string (ConcatenatePath paths)
			+++ " " +++ quoted_string path
			+++ " > "+++ quoted_string out_file_name
			+++ " \xb3 "+++ quoted_string errors_file_name	// \xb3 == >= ligature
	out_file_name
		= out_file_path startupdir 0
	errors_file_name
		= errors_file_path startupdir 0
	clear_cache_option
		| clearCache == Don`tClearCache
			= ""
		= " -clear_cache"

ConcatenatePath :: (List Pathname) -> String
/* old version
ConcatenatePath Nil             = ""
ConcatenatePath (path :! rest ) = path +++ ";" +++ ConcatenatePath rest
*/
ConcatenatePath ss
	# s = createArray (sSize ss) ','
	= sUpdate 0 s ss
where
	sSize Nil = 0
	sSize (string :! Nil) = size string
	sSize (string :! rest) = size string + 1 + sSize rest
	
	sUpdate i s Nil = s
	sUpdate i s (string :! Nil)
		# (_,s) = sU (size string) i 0 s string
		= s
	sUpdate i s (string :! rest)
		# (i,s) = sU (size string) i 0 s string
		# i = inc i
		= sUpdate i s rest
	
	sU l i j s h
		| j >= l = (i,s)
		# s = update s i h.[j]
		= sU l (inc i) (inc j) s h

ReadTypesInfo :: !Bool !Pathname !*Files -> ((!Bool,!(List String)),!*Files)
ReadTypesInfo readtypes	path env
	| not readtypes
		= ((False,Nil),env)
	#	(opened,file,env)			= fopena path FReadText env
	| not opened
		= ((False,Nil),env)
	#	(typelist,types_read,file`)	= ReadTypeMsg file
		(_,env)						= fclose file` env
	= ((types_read,typelist),env)
	

ReadTypeMsg :: !*File -> (!List String,!Bool,!*File)
ReadTypeMsg file
	#	(string,file)					= freadline file
		(eof,file)						= fend file
	| eof && IsTypeSpec string
		= (Strip string :! Nil,True,file) // DvA (LastStrings string,True,file)
	| eof
		= (Nil,False,file)
	#	(typeslist,types_read,file)	= ReadTypeMsg file
	= (Strip string :! typeslist,types_read,file) // DvA (ReplaceLastChar string :! typeslist,types_read,file)

Strip "" = ""
Strip s
	#! last = dec (size s)
	#! char = s.[last]
	| char == '\n' || char == '\r'
		= Strip (s % (0,dec last))
	= s

ReadErrorsAndWarnings :: !Pathname !*Files -> ((!CompilerMsg, !Bool, !(List String)), !*Files)
ReadErrorsAndWarnings path env
	#	(opened,file,env)	= fopenb path FReadText env
	| not opened
		= ((SyntaxError,False,Nil),env)
	#	(errors,errors_and_warnings_read,errlist,file`) = ReadErrorAndWarningMessages file
		(_,env) = fclose file` env
	= ((errors,errors_and_warnings_read,errlist),env)
	

ReadErrorAndWarningMessages :: !*File -> (!CompilerMsg,!Bool,!List String,!*File)
ReadErrorAndWarningMessages file
	#!	(string, file1)					= freadline file
		(eof,file2)						= fend file1
		(is_import_error13,path13)		= IsImportError13 string
		(is_import_error20,path20)		= IsImportError20 string
	# (is_import_error,path) = case is_import_error13 of
			True	-> (is_import_error13,path13)
			_		-> (is_import_error20,path20)
	| eof
		#!	not_empty_or_newline 		= (size string)<>0 && string.[0]<>'\n'
		= (if is_import_error (Patherror path) SyntaxError,not_empty_or_newline,Strip string :! Nil,file2)
	#	(path_error,_/*errors_and_warnings_read*/,errlist,file3) = ReadErrorAndWarningMessages file2
	= (if is_import_error (Patherror path) path_error,True,Strip string:!errlist,file3)


MakeCompilerOptionsString :: !CompileOrCheckSyntax !Bool !Bool !Bool !CompilerOptions -> String
MakeCompilerOptionsString compileOrCheckSyntax projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic
			{neverMemoryProfile, neverTimeProfile,sa,gw,gc,listTypes,attr,reuseUniqueNodes}
	= options
where 
	memoryProfileSwitch
		| (not neverMemoryProfile && projectMemoryProfiling)
		|| projectEagerOrDynamic
			= " -desc"		// of " -pm" vroeger???
			= ""
	timeProfileSwitch
		| not neverTimeProfile && projectTimeProfiling
			= " -pt"
			= ""
	dynamicLinkSwitch
		| projectEagerOrDynamic
			= " -exl"
			= ""
	strictness
		| sa
			= ""
			= " -sa"
	warnings
		| gw
			= ""
			= " -w"
	comments
		| gc
			= " -d"
			= ""
	listtypes
		| listTypes == InferredTypes
			= " -lt"
		| listTypes == AllTypes
			= " -lat"
		| listTypes == StrictExportTypes
			= " -lset"
			= ""
	show_attr
		| attr
			= ""
			= " -lattr"
	checksyntax
		| compileOrCheckSyntax == SyntaxCheck
			= " -c"
			= ""
	reuse
		| reuseUniqueNodes
			= " -ou"
			= ""

	options		= checksyntax +++ timeProfileSwitch +++ memoryProfileSwitch +++ dynamicLinkSwitch +++ strictness +++
						warnings +++ comments +++listtypes+++show_attr+++reuse+++" "
	

/* Generates code for the given file:
*/	

CodeGen	::	!String !(WindowFun *(PSt .l)) !CodeGenerateAsmOrCode !Pathname !Bool !CodeGenOptions !ApplicationOptions !Pathname !*(PSt .l)
			-> (!*(PSt .l), !Pathname, !Bool)
CodeGen cgen` wf genAsmOrCode path timeprofile cgo=:{tp} ao startupdir ps
//	# (cgen_ok,cgen)				= mangleGenerator cgen` startupdir
	# (cgen_ok,cgen,name,signature)	= mangleGenerator cgen` startupdir
	| not cgen_ok
		# ps				= wf [cgen] ps
		= (ps, "", False)

//	#	(objpath,ps)		= accFiles (MakeObjSystemPathname tp path) ps
	#	objpath				= MakeObjSystemPathname tp path
		path_without_suffix	= RemoveSuffix path
		command				= cgen /*"cg"*/ +++ MakeCodeGenOptionsString genAsmOrCode /*False*/timeprofile cgo 
								+++ " " +++ (quoted_string path_without_suffix)
								+++ " > out \xb3 errors" 
  		errorsfilename		= startupdir +++ toString dirseparator +++ "errors"
//	# signature = CleanCompilerSignature	// XOXOXOX
//	# name		= CleanCompilerName			// XOXOXOX
  	#	(error_code,error_n,output_string)
  							= send_command_to_clean_compiler_cg signature name command Wait
  	| error_code <> 0
		= ( wf (	[  "Unable to run code generator: "+++toString error_code
					]) ps
		, objpath
		, False
		)
	| size output_string <> 0
		= ( wf ( [output_string]) ps
		, objpath
		, error_n == 0
		)
	=  (ps,objpath,error_n==0)

StartCodeGenerator	::	!String !(WindowFun *(PSt .l)) !CodeGenerateAsmOrCode !Pathname !Int !Bool !CodeGenOptions !ApplicationOptions !Pathname !*(PSt .l)
			-> (!Bool,!Pathname,!*(PSt .l))
StartCodeGenerator cgen` wf genAsmOrCode path slot timeprofile cgo=:{tp} ao startupdir ps
	# (cgen_ok,cgen,name,signature)	= mangleGenerator cgen` startupdir
	| not cgen_ok
		# ps				= wf [cgen] ps
		= (False, "", ps)

//	# (objpath,ps)		= accFiles (MakeObjSystemPathname tp path) ps
	# objpath			= MakeObjSystemPathname tp path
	  path_without_suffix	= RemoveSuffix path
// 	  errorsfilename		= errors_file_path startupdir slot
	  command				= /*cgen*/ "cg"
								+++" -id "+++toString slot
								+++ MakeCodeGenOptionsString genAsmOrCode /*False*/timeprofile cgo
								+++ " " +++ (quoted_string path_without_suffix)
								+++ " > out \xb3 errors"
  	  name					= CleanCompilerName
  	  signature				= clean_compiler_signature slot
  	  (error_code,error_n,output_string) = send_command_to_clean_compiler signature name command NoWait
  	| error_code<>0
		# ps = wf ["Unable to run code generator: "+++toString error_code] ps
		= (False,objpath,ps)
	| size output_string <> 0
		# ps = wf [output_string] ps
		= (error_n == 0,objpath,ps)
	= (error_n==0,objpath,ps)

MakeCodeGenOptionsString genAsmOrCode timeprofile {ci,cs}
	= checkindex+++checkstack+++genasm+++timeProfileSwitch
where
	checkindex	| ci = " -ci"; = ""
	checkstack	| cs = " -os"; = ""
	timeProfileSwitch | timeprofile= " -pt"; = ""
	genasm		= case genAsmOrCode of
						AsmGeneration -> " -a"
						_ -> ""
	
/* Links the given file:
*/

Link ::	!String !(WindowFun *(PSt .l)) !Pathname !ApplicationOptions
		!Pathname !(List Pathname) !(List Pathname) !(List Pathname) !Bool 
		!Bool !Bool !Bool !String !Bool !String !Pathname !*(PSt .l)
		 -> (!*(PSt .l),!Bool)
Link linker` winfun path
		applicationOptions=:{fs,fn,em,ss,hs,initial_heap_size,profiling,heap_size_multiple,o,memoryProfilingMinimumHeapSize=minheap}
		optionspathname library_file_names object_file_names
		static_libraries static gen_relocs gen_linkmap
		link_resources resource_path gen_dll dll_names startupdir ps
	# (ok,linker)				= mangleLinker linker` startupdir
	| not ok
		# ps					= winfun [linker] ps
		= (ps,False)
	# flags						= ApplicationOptionsToFlags applicationOptions
	# optdirpath				= RemoveFilename optionspathname
	# ((ok,pd_optdirpath),ps)	= pd_StringToPath optdirpath ps
	| not ok
		= (winfun ["Linker error: Unable to understand path: "+++optdirpath] ps,False)
	# ((err,_),ps)				= getDirectoryContents pd_optdirpath ps
	# (err,ps)					= case err of
									DoesntExist -> createDirectory pd_optdirpath ps
									err  -> (err,ps)
	| err <> NoDirError
		= (winfun ["Linker error: Unable to access or create: "+++optdirpath] ps,False)
	# (options_file_ok,ps)		= accFiles (write_options_file optionspathname flags hs ss initial_heap_size heap_size_multiple minheap) ps
	| not options_file_ok
		= (winfun ["Linker error: Could not write the options object file: "+++optionspathname] ps,False)
	# linkopts =
		{ exe_path				= path
		, res_path				= ""
		, open_console			= o <> NoConsole
		, static_link			= static
		, gen_relocs			= gen_relocs
		, gen_linkmap			= gen_linkmap
		, link_resources		= False
		, object_paths			= /*optionspathname :!*/ (RemoveDup object_file_names)
	  	, dynamic_libs			= RemoveDup library_file_names
	  	, static_libs			= RemoveDup static_libraries
	  	, stack_size			= ss
	  	, gen_dll				= gen_dll
	  	, dll_names				= dll_names
	  	}
	# linkerpath				= RemoveFilename linker
	# linkoptspath				= MakeFullPathname linkerpath "linkopts"
	# linkerrspath				= MakeFullPathname linkerpath "linkerrs"
	# (err,ps)					= accFiles (WriteLinkOpts linkoptspath linkopts) ps
	| isJust err
		= (winfun (fromJust err) ps,False)

	# objectFileNames = StrictListToList (RemoveDup object_file_names)
	# libraryFileNames = StrictListToList (RemoveDup library_file_names)
	
//	#! ps = trace_n "Object Files:" ps
//	#! ps = trace_l objectFileNames ps
//	#! ps = trace_n "Library Files:" ps
//	#! ps = trace_l libraryFileNames ps
	# ((link_ok,link_errors),ps)
		=	accFiles (link_xcoff_files objectFileNames libraryFileNames path
				(fs,fn) hs heap_size_multiple ss flags em initial_heap_size minheap False) ps
	# (errtext,errlines)	= (link_errors, length link_errors);

	| errlines<>0
		= (winfun errtext ps,link_ok)
	=  (ps,link_ok)
/*
Link_ppc winfun path u_system_file_name paths defs
	applicationOptions=:{ss,fs,fn,hs,em,heap_size_multiple,initial_heap_size,profiling,profiling601,memoryProfilingMinimumHeapSize}
							linkOptions abcLinkObjFilePaths abcLinkLibraryPaths 
							prog=:{editor={startupinfo={startupdir},project}} ps
	# (u_startup_file_name,ps)
		= accFiles (MakeObjSystemPathname CurrentProcessor (MakeFullPathname system_directory_name startupModuleName)) ps
	# (u_library_object_file_name,ps)
		=	accFiles (MakeObjSystemPathname CurrentProcessor (MakeFullPathname system_directory_name "_library")) ps

	# ((link_ok,link_errors),ps)
		=	accFiles link ps
	# (errtext,errlines)	= StringToText (ListToStrictList link_errors);
	| errlines<>0
		= (winfun errtext prog ps,link_ok);
		= ((prog,ps),link_ok);
where
	link f = ((result, message), file)
	where
		(result, message, file)
			= link_xcoff_files objectFileNames libraryFileNames (MakeExecPathname path)
				(fs,fn) hs heap_size_multiple ss flags em initial_heap_size memoryProfilingMinimumHeapSize False f;


	objectFileNames
		=	removeDup	(	defaultObjects
						 ++ StrictListToList paths
						 ++ (StrictListToList linkOptions.extraObjectModules)
						 ++ (StrictListToList abcLinkObjFilePaths));	// MW P++

	startupModuleName
		| not profiling
			=	"_startup";
		| profiling601
			=	"_startupProfile601";
		| otherwise
			=	"_startupProfile";

	defaultObjects
		| linkOptions.useDefaultSystemObjects
			=	[u_startup_file_name, u_system_file_name, u_library_object_file_name];
		// otherwise
			=	[];

	libraryFileNames
		=	removeDup (defaultLibraries ++ (StrictListToList linkOptions.libraries)
										++ (StrictListToList abcLinkLibraryPaths));
	defaultLibraries
		| linkOptions.useDefaultLibraries
			= [	system_directory_name +++ ":library0",
							system_directory_name +++ ":library1",
							system_directory_name +++ ":library2"];		
		// otherwise
			=	[];
	flags					= ApplicationOptionsToFlags applicationOptions;

	system_directory_name = RemoveFilename u_system_file_name;
*/

DynLink :: !String !String !String !*(PSt .l) -> (Bool,*PSt .l)
DynLink linker prj_path startupdir ps
	= (False,ps)

//--- EXECUTE

Execute	::	!(WindowFun *env) !Pathname !ApplicationOptions *env -> (*env, !Bool)
Execute winfun path _ ps
	# (error_n,ps)	= Launch path ps
	| error_n >= 0
		= (ps,True)
		= (winfun ["Could not launch the application, MacOS error: "+++toString error_n] ps,False)

Launch :: !{#Char} !.a -> (!Int, !.a)
Launch execpath env
	# (error_n,_) = LaunchApplication execpath 0xC8000000 OSNewToolbox
	= (error_n, env)

//--- OTHER STUFF

QuitCleanCompiler :: !*(IOSt .l) -> *(IOSt .l)
// want to quit all launched Compilers in any env...???
// means we need to keep track of these somehow.
QuitCleanCompiler io
	# signature = CleanCompilerSignature	// XOXOXOX
	| send_quit_event_to_clean_compiler signature == 0
		= io;
		= io;

// necessary for Mac version?!
ClearCompilerCache :: !String !String !.a -> (!Int,!.a)
ClearCompilerCache cocl startupdir ps
	# (cocl_ok,cocl,name,signature)	= mangleCompiler cocl startupdir	// platform dependant mangling...
	| not cocl_ok = (-1,ps)
//	# signature = CleanCompilerSignature	// XOXOXOX
//	# name		= CleanCompilerName			// XOXOXOX
	# (os_error_code,_,_)	= send_command_to_clean_compiler_ca signature name "clear_cache" Wait
	= (os_error_code,ps)

ClearCompilerCaches :: !Int !.a -> (!Int,!.a)
ClearCompilerCaches n_compilers ps
	# os_error_code = clear_compiler_caches 0 0
		with
			clear_compiler_caches compiler_n previous_os_error_code
				| compiler_n<n_compilers
				  	#  name					= CleanCompilerName
				  	#  signature			= clean_compiler_signature compiler_n
					# (os_error_code,_,_)	= send_command_to_clean_compiler signature name "clear_cache" Wait
					| os_error_code==0 || previous_os_error_code<>0
						= clear_compiler_caches (compiler_n+1) previous_os_error_code
						= clear_compiler_caches (compiler_n+1) os_error_code
					= previous_os_error_code
	= (os_error_code,ps)

SendRepeatResult :: !Int !.a -> (!Int,!.a)
SendRepeatResult compiler_n ps
  	#  name					= CleanCompilerName
  	#  signature				= clean_compiler_signature compiler_n
	# (os_error_code,_,_) = send_command_to_clean_compiler signature name "repeat_result" NoWait
	= (os_error_code,ps)

Wait :== True
NoWait :== False

send_command_to_clean_compiler :: !String !String !String !Bool -> (!Int,!Int,!String);
send_command_to_clean_compiler signature name command wait_for_reply
	# (os_error_code,error_n,output_string)
		= send_command_to_clean_compiler0 signature command wait_for_reply;
	| error_n<>(-2)
		= (os_error_code,error_n,output_string);
	# (launch_error_n,_)
		= LaunchApplication name 0xCA000000 OSNewToolbox;	// Hangs under OS X?
	| launch_error_n>=0	
		= send_command_to_clean_compiler0 signature command wait_for_reply;
		= (os_error_code,-2,output_string);

//--

import UtilPatch
	
PatchableCleanCompilerSignature
	:== "#$@CLCOSIGNAT%*&ClCo\0";
CleanCompilerSignature
	=: PatchableValue "CLCOSIGNAT" PatchableCleanCompilerSignature;

PatchableCleanCompilerName
	:== "#$@CLCONAME  %*&Clean Compiler\0............................some extra space for long names (total 128 chars).......................";
CleanCompilerName
	=: PatchableValue "CLCONAME  " PatchableCleanCompilerName;

clean_compiler_signature slot
	| slot==0
		= CleanCompilerSignature
		# s=CleanCompilerSignature
		= {s.[0],s.[1],s.[2],toChar (slot+48)}

//--
import events
KAEApplicationDied	:== 0x6F626974; // 'obit'

send_command_to_clean_compiler0 :: !String !String !Bool-> (!Int,!Int,!String);
send_command_to_clean_compiler0 signature command wait
	| error_code1<>0
		= (error_code1,-1,"NewPtr failed");
//	# error_code2 = AECreateDesc TypeApplSignature "MPSX" descriptor; // Tool Server
	# error_code2
		= AECreateDesc TypeApplSignature signature descriptor;
	| error_code2<>0
		= (free_memory error_code2,-1,"AECreateDesc failed");
	# error_code3
		= AECreateAppleEvent KAEMiscStandards KAEDoScript descriptor KAutoGenerateReturnID KAnyTransactionID apple_event;
	| error_code3<>0
		= (free_descriptor_and_memory error_code3,-1,"AECreateAppleEvent failed");
	# error_code4
		= AEPutParamPtr apple_event KeyDirectObject TypeChar command;
	| error_code4<>0
		= (free_apple_event_and_desciptor_and_memory error_code4,-1,"AEPutParamPtr failed");
	# error_code5
		= case wait of
			True -> AESend apple_event result_apple_event KAEWaitReply KAENormalPriority KNoTimeOut 0 0;
//			True -> loop OSNewToolbox;
			_	 -> AESend apple_event 0 KAEQueueReply KAENormalPriority KNoTimeOut 0 0;
	| error_code5==(-609)
		= (free_apple_event_and_desciptor_and_memory error_code5,-2,"AESend failed");
	| error_code5==(-903)
		= (free_apple_event_and_desciptor_and_memory error_code5,-2,"need to add HighLevel event aware to SIZE resource of IDE...");
	| error_code5==(-1712)
		= (free_apple_event_and_desciptor_and_memory error_code5,-1,"AESend failed; Application died");
	| error_code5<>0
		= (free_apple_event_and_desciptor_and_memory error_code5,-1,"AESend failed");
	| not wait
		= (free_apple_event_and_desciptor_and_memory error_code5,0,"");
	# (error_code6,_,v1,_)
		= AEGetIntParamPtr result_apple_event KeyErrorNumber TypeLongInteger;
	# (error_code7,_,s2)
		= AEGetStringParamPtr result_apple_event KeyErrorString TypeChar result_string;
	# os_error_code
		= free_result_apple_event_and_apple_event_and_desciptor_and_memory error_code6 error_code7
	# error_n
		= if (error_code6<0) 0 v1
	# output_string
		= if (error_code7<>0) "" (result_string % (0,s2-1))
	= (os_error_code,error_n,output_string)
where
	loop tb
		# err	= trace_n` "TICK!" AESend apple_event result_apple_event KAEWaitReply KAENormalPriority (60) 0 0;
		| err==(-1712)
			# (avail,what,message,when,wherex,wherey,modifiers,tb) = EventAvail NetworkMask tb
			| avail && what == HighLevelEvent && message == KCoreEventClass && wherex == KAEApplicationDied
				= -1712
			= loop tb
		= err
	result_string = createArray 5120 '0';

	(memory,error_code1,_)
		= NewPtr (SizeOfAEDesc+SizeOfAppleEvent+SizeOfAppleEvent) 0;
	descriptor
		= memory;
	apple_event
		= memory+SizeOfAEDesc;
	result_apple_event
		= memory+SizeOfAEDesc+SizeOfAppleEvent;

	free_result_apple_event_and_apple_event_and_desciptor_and_memory error_code6 error_code7
		| error_code6==error_code6 && error_code7==error_code7
			= free_apple_event_and_desciptor_and_memory free_error_code;
	where
		free_error_code = AEDisposeDesc result_apple_event;

	free_apple_event_and_desciptor_and_memory error_code
		| error_code==0
			= free_descriptor_and_memory free_error_code;
		| free_error_code==0
			= free_descriptor_and_memory error_code;
			= free_descriptor_and_memory error_code;
	where
		free_error_code = AEDisposeDesc apple_event;

	free_descriptor_and_memory error_code
		| error_code==0
			= free_memory free_error_code;
		| free_error_code==0
			= free_memory error_code;
			= free_memory error_code;
	where
		free_error_code = AEDisposeDesc descriptor;

	free_memory error_code
		| error_code==0
			= if (free_error_code==255) 0 free_error_code;
		| free_error_code==0
			= error_code;
			= error_code;
	where
		(free_error_code,_)	= DisposPtr memory 0;

send_quit_event_to_clean_compiler :: !String -> Int;
send_quit_event_to_clean_compiler signature
	| error_code1<>0
		= error_code1;
	| error_code2<>0
		= free_memory error_code2;
	| error_code3<>0
		= free_descriptor_and_memory error_code3;
	| error_code4<>0
		= free_apple_event_and_desciptor_and_memory error_code4;
		= free_apple_event_and_desciptor_and_memory error_code4;
where
	(memory,error_code1,_) = NewPtr (SizeOfAEDesc+SizeOfAppleEvent+SizeOfAppleEvent) 0;

	descriptor=memory;
	apple_event=memory+SizeOfAEDesc;
	result_apple_event=memory+SizeOfAEDesc+SizeOfAppleEvent;

	error_code2 = AECreateDesc TypeApplSignature signature descriptor;
	error_code3 = AECreateAppleEvent KCoreEventClass KAEQuitApplication descriptor KAutoGenerateReturnID KAnyTransactionID apple_event;
	error_code4 = AESend apple_event result_apple_event KAENoReply KAENormalPriority KNoTimeOut 0 0;

	free_apple_event_and_desciptor_and_memory error_code
		| error_code==0
			= free_descriptor_and_memory free_error_code;
		| free_error_code==0
			= free_descriptor_and_memory error_code;
			= free_descriptor_and_memory error_code;
	where
		free_error_code = AEDisposeDesc apple_event;

	free_descriptor_and_memory error_code
		| error_code==0
			= free_memory free_error_code;
		| free_error_code==0
			= free_memory error_code;
			= free_memory error_code;
	where
		free_error_code = AEDisposeDesc descriptor;

	free_memory error_code
		| error_code==0
			= if (free_error_code==255) 0 free_error_code;
		| free_error_code==0
			= error_code;
			= error_code;
	where
		(free_error_code,_)	= DisposPtr memory 0;

//--
/*
trace_l [] s = trace_n "[]" s
trace_l [h:t] s
	#! s = trace_n ("["+++toString h) s
	= list t s
where
	list [] s = trace_n "]" s
	list [h:t] s
		#! s = trace_n (","+++toString h) s
		= list t s
*/
//--

// RWS ...

import StdMaybe

:: ThreadId
	:==	Int

CompileStartCommand :: !String !Bool !(WindowFun *env) !CompileOrCheckSyntax !Pathname !(List Pathname) !Int !Bool !Bool !Bool
				!CompilerOptions !Pathname !*env -> (!Bool, !*env) | FileEnv env
//CompileStartCommand _ _ winfun _ _ _ _ _ _ _ _ _ env = (False, winfun ["Async not supported."] env)
CompileStartCommand cocl write_module_times errwin compileOrCheckSyntax path paths slot projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic
					co=:{CompilerOptions | listTypes} startupdir ps
  	#  name					= CleanCompilerName
  	#  signature				= clean_compiler_signature slot
	# (error_code,error_n,ss) = trace_n command send_command_to_clean_compiler signature name command NoWait
	| error_code <> 0
		= ( False, errwin (	[ "Unable to run compiler: "+++cocl
					+++ "; "+++ toString error_code
					+++ "; "+++ toString error_n
					+++ "; "+++ ss
					]) ps
		)
     = (True,ps)
where
	write_module_times_string = if write_module_times " -wmt " " "
	clearCache = Don`tClearCache	// needs to be arg to compile command???
	command
		= "cocl"
			+++" -id "+++toString slot
			+++clear_cache_option +++ write_module_times_string
			+++ MakeCompilerOptionsString
					compileOrCheckSyntax
					projectMemoryProfiling
					projectTimeProfiling
					projectEagerOrDynamic
					co
        	+++ " -sl"
        	+++ " -P " +++ quoted_string (ConcatenatePath paths)
			+++ " " +++ quoted_string path
			+++ " > "+++ quoted_string out_file_name
			+++ " \xb3 "+++ quoted_string errors_file_name	// \xb3 == >= ligature
	out_file_name
		= out_file_path startupdir slot
	errors_file_name
		= errors_file_path startupdir slot
	clear_cache_option
		| clearCache == Don`tClearCache
			= ""
		= " -clear_cache"

CompileHandleExitCode :: !Int !String !String !Int !(WindowFun *env) !(WindowFun *env) !Pathname
				!ListTypes !*env -> (!*env,!Pathname,!CompilerMsg) | FileEnv env
//CompileHandleExitCode _ _ _ _ winfun _ _ _ env = (winfun ["Async not supported."] env, "", SyntaxError)
CompileHandleExitCode exitcode cocl startupdir slot errwin typewin path listTypes ps
	# out_file_name = out_file_path startupdir slot
	# errors_file_name = errors_file_path startupdir slot
	#	((type_text_not_empty,type_text),ps)
						= accFiles (ReadTypesInfo (listTypes<>NoTypes) out_file_name) ps
		((errors,errors_and_messages_not_empty,errors_and_messages),ps)
						= accFiles (ReadErrorsAndWarnings errors_file_name) ps
//		(abcpath,ps)	= accFiles (MakeABCSystemPathname path) ps
		abcpath			= MakeABCSystemPathname path
		ps				= case errors_and_messages_not_empty of
							True	-> trace_n "errwin" errwin (StrictListToList errors_and_messages) ps
							False	-> trace_n "ok" ps
		ps				= case type_text_not_empty of
							True	-> typewin (StrictListToList type_text) ps
							False	-> ps
     = (ps,abcpath,if (exitcode==1) CompilerOK errors)

:: CompilePollCompletedResult = NoFinishedCompiler | UnknownFinishedCompiler | FinishedCompiler !Int !Int
//:: CompilePollCompletedResult :== Maybe !(!Int,!Int)

CompilePollCompleted :: !*env -> (!CompilePollCompletedResult, !*env) | FileEnv env
//CompilePollCompleted env = (Nothing, env)
CompilePollCompleted env
	# (compiler_id,exit_code) = get_finished_compiler_id_and_exit_code
//	= abort ("CompilePollCompleted "+++toString compiler_id+++" "+++toString exit_code)
//	| trace_tn ("CompilePollCompleted "+++toString compiler_id+++" "+++toString exit_code)
	| compiler_id<0
		| exit_code==1
			= (UnknownFinishedCompiler,env)
			= (NoFinishedCompiler,env)
//	| trace_tn ("CompilePollCompleted "+++toString compiler_id+++" "+++toString exit_code)
//		= (FinishedCompiler compiler_id exit_code,env);
		= (FinishedCompiler compiler_id exit_code,env);

get_finished_compiler_id_and_exit_code :: (!Int/*compiler_id*/,!Int/*exit_code*/);
get_finished_compiler_id_and_exit_code = code {
	ccall get_finished_compiler_id_and_exit_code ":II"
 }

// ... RWS

// JVG ...

:: CompilingInfo = CompilingInfo

InitCompilingInfo :: !*CompilingInfo
InitCompilingInfo = CompilingInfo

ExitCleanCompiler :: !*(!*CompilingInfo,*env) -> *(!*CompilingInfo,*env)
ExitCleanCompiler (i,e) = (i,e)

CompilePersistent ::
	!String					// cocl
	!Bool
	!(WindowFun *env)		// errwin
	!(WindowFun *env)		// typewin
	!CompileOrCheckSyntax	// compileOrCheckSyntax
	!Pathname				// path
	!(List Pathname)		// paths
	!Bool					// projectHeapProfiling
	!Bool					// projectTimeProfiling
	!Bool					// projectEagerOrDynamic
	!CompilerOptions		// compileroptions
	!Pathname				// startupdir
	!*CompilingInfo			// compiler state
	!*env					// env
	-> (!*CompilingInfo,!(!*env, !Pathname, !CompilerMsg))
	| FileEnv env
CompilePersistent cocl write_module_times errwin typewin compileOrCheckSyntax path paths projectHeapProfiling projectTimeProfiling
	projectEagerOrDynamic compileroptions startupdir state env
	= (state,(errwin ["Persistent not supported."] env,"",SyntaxError))
// ... JVG

//--

from UtilIO import GetShortPathName
/*
mangleCompiler ccstring` startupdir
	# (ccstring`,opts)		= splitOptions ccstring`
	# (shortOK,ccstring)	= GetShortPathName (startupdir +++ "\\" +++ ccstring` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ "\\" +++ ccstring`) +++ "'."
		= (False,line)
	# ccstring				= ccstring % (0, size ccstring - 2) +++ opts
	= (True,ccstring)
*/
from DodoUtil import sSplit

mangleCompiler ccstring startupdir
	# (name,rest)			= sSplit ';' ccstring
	# (sign,cocl)			= sSplit ';' rest
	# name = case name of
				""	-> "Clean Compiler"//\0"
				n	-> n
	# sign = case sign of
				""	-> "ClCo"//\0"
				s	-> s
	# cocl = case cocl of
				""	-> "cocl"
				c	-> c
	= (True,cocl,name,sign)
/*
mangleGenerator cgen` startupdir
	# (cgen`,opts)			= splitOptions cgen`
	# (shortOK,cgen)		= GetShortPathName (startupdir +++ "\\" +++ cgen` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ "\\" +++ cgen`) +++ "'."
		= (False,line)
	# cgen = cgen % (0, size cgen - 2) +++ opts
	= (True,cgen)
*/
mangleGenerator cgstring startupdir
	# (name,rest)			= sSplit ';' cgstring
	# (sign,cgen)			= sSplit ';' rest
	# name = case name of
				""	-> "Clean Compiler"//\0"
				n	-> n
	# sign = case sign of
				""	-> "ClCo"//\0"
				s	-> s
	# cgen = case cgen of
				""	-> "cg"
				c	-> c
	= (True,cgen,name,sign)

mangleLinker linkstr` startupdir
	# (linkstr`,opts)		= splitOptions linkstr`
	# (shortOK,linkstr)		= GetShortPathName (startupdir +++ toString dirseparator +++ linkstr` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ toString dirseparator +++ linkstr`) +++ "'."
		= (False,line)
	# linkstr = linkstr % (0, size linkstr - 2) +++ opts
	= (True,linkstr)

splitOptions str
	| first_q >= len_str	= (str,"")
	| last_q >= len_str		= (first_str,"")
	= (first_str,last_str)
where
	first_str =  str%(0,dec first_q)
	last_str = str % (inc first_q, dec last_q)

	len_str = size str
	
	first_q			= FindQuoteChar str len_str 0
	last_q			= FindQuoteChar str len_str (inc first_q)
		
	//	FindQuoteChar	:: !String !Int !Int -> Int;
	FindQuoteChar str len pos	= FindChar '\"' str len pos;

	FindChar	:: !Char !.String !.Int !Int -> Int;
	FindChar c line linelen pos
		| pos >= linelen		=  pos;
		| c ==  line.[pos]		=  pos;
								=  FindChar c line linelen (inc pos);
	
