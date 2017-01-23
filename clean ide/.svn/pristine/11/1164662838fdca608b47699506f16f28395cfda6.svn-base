implementation module PmCleanSystem

import StdArray, StdBool, StdChar, StdFunc, StdInt, StdList, StdEnum
import StdSystem, StdPStClass, StdMaybe
from StdMisc import undef

import Directory

import PmCompilerOptions, UtilStrictLists
import PmPath
import UtilNewlinesFile
import WriteOptionsFile

from PmParse import IsTypeSpec, IsImportError13, IsImportError20
from clCCall_12 import winLaunchApp, winLaunchApp2, winCallProcess, winMakeCString, winReleaseCString, :: OSToolbox, :: CSTR
from linkargs import ReadLinkErrors,WriteLinkOpts,:: LinkInfo`(..),:: LPathname
import thread_message
import lib
//import asynclaunch

import UtilIO

//import dodebug
trace_n _ f :== f

from Platform import TempDir
tooltempdir =: trace_n ("Tooltempdir",TempDir) TempDir

//--

// kernel_library is required by the _system _startup combo and should be encoded there
standardStaticLibraries :: !Processor !LinkMethod -> List String
standardStaticLibraries _ method
	= case method of
		LM_Static	-> ("kernel_library" :! morelibs)
		LM_Dynamic	-> ("kernel_library" :! "StaticClientChannel_library" :! morelibs)
/*
		LM_Eager	-> ("kernel_library" :! "StaticClientChannel_library" :! Nil)
		LM_Dynamic	-> ("kernel_library" :! "ClientChannel_library" :! Nil)
*/
morelibs	// Note that these dependencies are introduced by StdEnv and should be encoded there.
	= "user_library" :! "gdi_library" :! "comdlg_library" :! Nil

standardObjectFiles :: !Bool !Bool !Processor !Bool -> List String
standardObjectFiles stack_traces profiling _ use_64_bit_processor
	#! startup1_file = if stack_traces "_startup1Trace.o"
					  (if profiling "_startup1Profile.o" "_startup1.o")
	| not use_64_bit_processor
		= ("_startup0.o" :! startup1_file :! "_startup2.o" :! "_system.o" :! Nil)
		= ("_startup0.o" :! startup1_file :! "_startup2.o" :! "_startup3.o" :! "_startup4.o" :! "_system.o" :! Nil)

//-- interface to static libraries...

getLibs :: ![String] !*Files -> (!(![String],![String]),!*Files)
getLibs [] files = (([],[]),files)
getLibs [lib:libs] files
	# (errs,slibs,files)		= getLib lib files
	# ((errs`,slibs`),files)	= getLibs libs files
	= ((errs++errs`,slibs++slibs`),files)
getLib lib files
	# (errs,slibs,files)	=  OpenArchive lib files
	# slibs					= map RemoveSuffix slibs
	= (errs,slibs,files)

:: CompilerProcessHandlesAndId = {
	compiler_thread_id :: !Int,
	compiler_thread_handle :: !Int,
	compiler_process_handle :: !Int
   }

:: CompilerProcessIds :== [CompilerProcessHandlesAndId]

NoCompilerProcessIds :: CompilerProcessIds
NoCompilerProcessIds = []

ClearCompilerCache :: !String !String !.a -> (!Int,!.a)
ClearCompilerCache _ _ ps = (0,ps)

ClearCompilerCaches :: !CompilerProcessIds !.a -> (!Int,!.a)
ClearCompilerCaches _ ps = (0,ps)

QuitCleanCompiler :: !Bool !CompilerProcessIds !.env -> .env
QuitCleanCompiler async compiler_process_ids io
	| async
		= quit_compilers compiler_process_ids io;
		with
			quit_compilers [{compiler_thread_id,compiler_process_handle}:compiler_process_ids] io
				# wm_number=get_message_number;
				# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("exit\0")
				| r==r
					= quit_compilers compiler_process_ids io;
			quit_compilers [] io
				= io;
	= io;

ExitCleanCompiler :: !*(!*CompilingInfo,*env) -> *(!*CompilingInfo,*env)
ExitCleanCompiler prog=:(CompilingInfo (CompilerProcess compiler_thread_id compiler_thread_handle compiler_process_handle), ps)
	# wm_number=get_message_number;
	# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("exit\0")
	| /*trace_tn ("ExitCleanCompiler "+++toString r+++"\n") &&*/ r==0
		= prog
		= (CompilingInfo NoCompiler,ps);
ExitCleanCompiler prog
	= prog

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
/*
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
*/
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
	
::	WindowFun env :== ([String]) -> env -> env

//-- Synchronous compilation stuff...

Compile ::
	!String !Bool !Bool !(WindowFun *env) !(WindowFun *env) !CompileOrCheckSyntax !Pathname
	!(List Pathname) !Bool !Bool !Bool !CompilerOptions !Pathname !CompilerProcessIds !*env
	-> (!Pathname,!CompilerMsg,!CompilerProcessIds,!*env)
	| FileEnv env
Compile
	cocl` use_compiler_process_ids write_module_times errwin typewin compileOrCheckSyntax path paths projectMemoryProfiling
	projectTimeProfiling projectEagerOrDynamic co=:{CompilerOptions | listTypes}
	startupdir compiler_process_ids ps
	# (cocl_ok,cocl,cocldir)	= mangleCompiler cocl` startupdir	// platform dependant mangling...
	| not cocl_ok
		# ps					= errwin [cocl] ps
		= ("",SyntaxError,compiler_process_ids,ps)
	#	out_file_name		= out_file_path tooltempdir dummy_slot
		errors_file_name	= errors_file_path tooltempdir dummy_slot
	#	command				= cocl +++ write_module_times_string +++ CompileBuildCommand
					out_file_name errors_file_name compileOrCheckSyntax path paths
					projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic co
		(didit, exitcode, os4) = CallProcess command [] cocldir "" "" "" 99
	    diditall               = if (os4 == 99) didit didit
	| not diditall
		# ps	= errwin (["Error: Unable to run compiler: "+++cocl +++ " :"+++toString exitcode]) ps
		= ("",SyntaxError,compiler_process_ids,ps)
	# (path,mess,env) =	CompileHandleExitCode exitcode cocl tooltempdir dummy_slot errwin typewin path listTypes ps
	= (path,mess,compiler_process_ids,env)
where
	dummy_slot = 0
	write_module_times_string = if write_module_times " -wmt " " "

mangleCompiler ccstring` startupdir
	# (ccstring`,rem)			= splitOptions ccstring`
	# (opts,opts`)				= splitOptions rem
	# (shortOK,ccstring)		= GetShortPathName (startupdir +++ "\\" +++ ccstring` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ "\\" +++ ccstring`) +++ "'."
		= (False,line,"")
	# cocl = ccstring % (0, size ccstring - 2) +++. opts +++. opts`
	# cocldir = RemoveFilename (ccstring % (0, size ccstring - 2))
	= (True,cocl,cocldir)

mangleCompiler2 ccstring` startupdir
	# (ccstring`,rem)			= splitOptions ccstring`
	# (opts,opts`)				= splitOptions rem
	# (shortOK,ccstring)		= GetShortPathName (startupdir +++ "\\" +++ ccstring` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ "\\" +++ ccstring`) +++ "'."
		= (False,line,"","","")
	# cocl = ccstring % (0, size ccstring - 2)
	# cocldir = RemoveFilename cocl
	= (True,cocl,cocldir,opts,opts`)

//-- Asynchronous compilation stuff...

:: ThreadId
	:==	Int
:: ExitCode
	:== Int

/*
CompileStartCommand :: !String !Bool !(WindowFun *env) !CompileOrCheckSyntax !Pathname !(List Pathname) !Int !Bool !Bool !Bool
				!CompilerOptions !Pathname !CompilerProcessIds !*env -> (!Bool,!CompilerProcessIds,!*env) | FileEnv env
CompileStartCommand cocl` write_module_times errwin compileOrCheckSyntax path paths slot projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic
					co startupdir compiler_process_ids ps
	# (cocl_ok,cocl,cocldir)	= mangleCompiler cocl` startupdir	// platform dependant mangling...
	| not cocl_ok
		# ps					= errwin [cocl] ps
		= (False,compiler_process_ids,ps)
	#	out_file_name		=  out_file_path tooltempdir slot
		errors_file_name	=  errors_file_path tooltempdir slot
	#	command
			=	cocl +++ " " +++ CompileBuildCommand out_file_name errors_file_name compileOrCheckSyntax path paths
					projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic co
		(didit, os4) = AsyncCallProcess (command +++ "\0") slot 99
	    diditall = if (os4 == 99) didit didit
	| not diditall
		=	(False,compiler_process_ids,errwin ["Error: Unable to run compiler: "+++cocl] ps)
	// otherwise
		=	(True,compiler_process_ids,ps)
where
	write_module_times_string = if write_module_times " -wmt " " "
*/

CompileStartCommand :: !String !Bool !(WindowFun *env) !CompileOrCheckSyntax !Pathname !(List Pathname) !Int !Bool !Bool !Bool
						!CompilerOptions !Pathname !CompilerProcessIds !*env
										 -> (!Bool,!CompilerProcessIds,!*env) | FileEnv env
CompileStartCommand cocl` write_module_times errwin compileOrCheckSyntax path paths slot projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic
					co startupdir compiler_process_ids ps
	# (cocl_ok,cocl,cocl_dir,cocl_startup,options)	= mangleCompiler2 cocl` startupdir	// platform dependant mangling...
	| not cocl_ok
		# ps = errwin [cocl] ps
		= (False,compiler_process_ids,ps)
	# out_file_name		=  out_file_path tooltempdir slot
	# errors_file_name	=  errors_file_path tooltempdir slot
	# cocl_arguments
		= " -id " +++toString slot+++" "+++options +++ write_module_times_string +++. 
			CompileBuildCommand out_file_name errors_file_name compileOrCheckSyntax path paths projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic co
	# (compile_ok,compiler_process_ids,ps) = start_compile_with_cache cocl slot cocl_dir cocl_startup cocl_arguments compiler_process_ids ps;
	| not compile_ok
  		# ps = errwin ["Error: Unable to run compiler: "+++cocl] ps
  		= (False,compiler_process_ids,ps)
	= (True,compiler_process_ids,ps)
where
	write_module_times_string = if write_module_times " -wmt " " "

start_compile_with_cache :: String Int String String String CompilerProcessIds *env -> (!Bool,!CompilerProcessIds,!*env)
start_compile_with_cache path slot directory startup_arguments arguments compiler_process_ids ps
	| slot<length compiler_process_ids
		# compiler_handles_and_id = compiler_process_ids !! slot
		= start_compile_with_cache2 path compiler_handles_and_id directory arguments compiler_process_ids ps
	# thread_id=get_current_thread_id;
	# begin_arguments=startup_arguments+++" -ide "+++int_to_hex thread_id;
	# (r,compiler_thread_id,compiler_thread_handle,compiler_process_handle) = start_compiler_process (path+++"\0") (directory+++"\0") (path+++" "+++begin_arguments+++"\0");
	| r==0
		= (False,compiler_process_ids,ps)
	# compiler_handles_and_id = {compiler_thread_id=compiler_thread_id,compiler_thread_handle=compiler_thread_handle,compiler_process_handle=compiler_process_handle}
	# compiler_process_ids = compiler_process_ids++[compiler_handles_and_id]
	= start_compile_with_cache2 path compiler_handles_and_id directory arguments compiler_process_ids ps

start_compile_with_cache2 :: {#.Char} CompilerProcessHandlesAndId {#.Char} {#.Char} CompilerProcessIds *env -> (!Bool,!CompilerProcessIds,!*env)
start_compile_with_cache2 path {compiler_thread_id,compiler_thread_handle,compiler_process_handle} directory arguments compiler_process_ids ps
	# wm_number=get_message_number
	# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("cocl "+++arguments+++"\0")
	| r==0
		= (False,compiler_process_ids,ps)
	= (True,compiler_process_ids,ps)

:: CompilePollCompletedResult = NoFinishedCompiler | UnknownFinishedCompiler | FinishedCompiler !Int !Int
/*
CompilePollCompleted ps
	# (ok, exitCode, slot, os)
		=	AsyncPollCompleted 99
//	| trace_n ("CompilePollCompleted ok=" +++ toString ok +++ " slot/r=" +++ toString slot) ok
	| ok
		=	//wait 100
			(FinishedCompiler slot exitCode, ps)
	// not ok
		=	(NoFinishedCompiler, ps)
*/

CompilePollCompleted :: !*env -> (!CompilePollCompletedResult, !*env) | FileEnv env
CompilePollCompleted ps
	# (compiler_id,exit_code) = get_finished_compiler_id_and_exit_code
	| compiler_id<0
		= (NoFinishedCompiler,ps)
		= (FinishedCompiler compiler_id exit_code,ps);

get_finished_compiler_id_and_exit_code :: (!Int/*compiler_id*/,!Int/*exit_code*/);
get_finished_compiler_id_and_exit_code = code {
	ccall get_finished_compiler_id_and_exit_code ":II"
 }

//-- Persistent compilation stuff...synchronous for now...

CompilePersistent ::
	!String !Bool !(WindowFun *env) !(WindowFun *env) !CompileOrCheckSyntax !Pathname
	!(List Pathname) !Bool !Bool !Bool !CompilerOptions !Pathname !*CompilingInfo !*env
	-> (!*CompilingInfo,!(!*env, !Pathname, !CompilerMsg))
	| FileEnv env
CompilePersistent
	cocl` write_module_times errwin typewin compileOrCheckSyntax path paths projectHeapProfiling
	projectTimeProfiling projectEagerOrDynamic co=:{CompilerOptions | listTypes}
	startupdir cstate env

	# (cocl_ok,cocl,cocl_dir,cocl_startup,options)	= mangleCompiler2 cocl` startupdir	// platform dependant mangling...
	| not cocl_ok
		# env					= errwin [cocl] env
		= (cstate,(env,"",SyntaxError))

	#	out_file_name		=  out_file_path tooltempdir dummy_slot
		errors_file_name	=  errors_file_path tooltempdir dummy_slot
	# cocl_arguments
		= options +++ write_module_times_string +++. CompileBuildCommand out_file_name errors_file_name compileOrCheckSyntax path paths
			projectHeapProfiling projectTimeProfiling projectEagerOrDynamic co
//	# cstate = NotCompiling
//	# cocl = startupdir+++toString dirseparator +++"cocl.exe";
	# (compile_ok,exitcode,(cstate,env)) = compile_with_cache cocl cocl_dir cocl_startup cocl_arguments (cstate,env);
	| not compile_ok
  		# env = errwin ["Error: Unable to run compiler: "+++cocl +++ " :"+++toString exitcode] env
  		= (cstate,(env,"",SyntaxError))
	# (path,mess,env) = CompileHandleExitCode exitcode cocl tooltempdir dummy_slot  errwin typewin path listTypes env
	=	(cstate,(env,path,mess))
where
	dummy_slot = 0
	write_module_times_string = if write_module_times " -wmt " " "

//-- Generic Compilation stuff...

CompileBuildCommand :: !String !String !CompileOrCheckSyntax !Pathname !(List Pathname) !Bool !Bool !Bool
				!CompilerOptions -> String
CompileBuildCommand out_file_name errors_file_name compileOrCheckSyntax path paths
					projectHeapProfiling projectTimeProfiling projectEagerOrDynamic co
	= MakeCompilerOptionsString
		compileOrCheckSyntax
		projectHeapProfiling
		projectTimeProfiling
		projectEagerOrDynamic
		co
		+++ (quoted_string path)
        +++ " -P " +++ quoted_string (ConcatenatePath paths)
		+++ " -RE "+++ quoted_string errors_file_name
		+++ " -RO "+++ quoted_string out_file_name;

CompileHandleExitCode :: !Int !String !String !Int !(WindowFun *env) !(WindowFun *env) !Pathname
				!ListTypes !*env -> (!Pathname,!CompilerMsg,!*env) | FileEnv env
CompileHandleExitCode exitcode cocl startupdir slot errwin typewin path
					listTypes ps
	#	out_file_name		=  out_file_path tooltempdir slot
		errors_file_name	=  errors_file_path tooltempdir slot
	#	((type_text_not_empty,type_text),ps)
			= accFiles (ReadTypesInfo (listTypes<>NoTypes) out_file_name) ps
		((errors,errors_and_messages_not_empty,errors_and_messages),ps)
			= accFiles (ReadErrorsAndWarnings errors_file_name) ps
	| exitcode <> 0 && not errors_and_messages_not_empty =	// werkt dit ook voor persistent versie?
		( ""
		, SyntaxError
		, errwin (	[  "Error: Compiler crashed: "+++cocl
					: if (errors == CompilerOK) ["Unable to open Errors file"] []
					]) ps
		)
	#	abcpath			= MakeABCSystemPathname path
		ps				= (if type_text_not_empty (typewin (StrictListToList type_text)) id) ps
		ps				= (if errors_and_messages_not_empty (errwin (StrictListToList errors_and_messages)) id) ps
		errors			= case exitcode of
							0	-> CompilerOK
							_	-> errors
     = (abcpath,errors,ps)

out_file_path :: String Int -> String
out_file_path tooltempdir slot
	=	file_path tooltempdir "out" slot

errors_file_path :: String Int -> String
errors_file_path tooltempdir slot
	=	file_path tooltempdir "errors" slot

file_path :: String String Int -> String
file_path dir base_name slot
	=	dir +++ toString dirseparator +++ base_name +++ (if (slot == 0) "" (toString slot))

ConcatenatePath :: (List Pathname) -> String
/* old version
ConcatenatePath Nil             = ""
ConcatenatePath (path :! rest ) = path +++ ";" +++ ConcatenatePath rest
*/
ConcatenatePath ss
	# s = createArray (sSize ss) ';'
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
	#	(opened,file,env)			= fopen path FReadText env
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
		= (Strip string :! Nil,True,file)
	| eof
		= (Nil,False,file)
	#	(typeslist,types_read,file)	= ReadTypeMsg file
	= (Strip string :! typeslist,types_read,file)

Strip "" = ""
Strip s
	#! last = dec (size s)
	#! char = s.[last]
	| char == '\n' || char == '\r'
		= Strip (s % (0,dec last))
	= s

ReadErrorsAndWarnings :: !Pathname !*Files -> ((!CompilerMsg, !Bool, !(List String)), !*Files)
ReadErrorsAndWarnings path env
	#	(opened,file,env)	= fopen path FReadText env
	| not opened
		= ((CompilerOK,False,Nil),env)
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
	#	(path_error,_,errlist,file3) = ReadErrorAndWarningMessages file2
	= (if is_import_error (Patherror path) path_error,True,Strip string:!errlist,file3)


MakeCompilerOptionsString :: !CompileOrCheckSyntax !Bool !Bool !Bool !CompilerOptions -> String
MakeCompilerOptionsString compileOrCheckSyntax projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic
			{neverMemoryProfile, neverTimeProfile,sa,gw,gc,listTypes,attr,reuseUniqueNodes,fusion}
	= options
where 
	memoryProfileSwitch
		| (not neverMemoryProfile && projectMemoryProfiling)
		|| projectEagerOrDynamic
			= " -desc"
			= ""
	timeProfileSwitch
		| not neverTimeProfile && projectTimeProfiling
			= " -pt"
			= ""
	dynamicLinkSwitch
		| projectEagerOrDynamic
			= " -exl -dynamics"
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
	add_fusion_option s = if fusion (s+++" -fusion") s;
	
	options		= add_fusion_option (checksyntax +++ timeProfileSwitch +++ memoryProfileSwitch +++ dynamicLinkSwitch
									+++ strictness +++ warnings +++ comments +++listtypes+++show_attr+++reuse)
				  +++" "
	

/* Generates code for the given file:
*/	

watcom_quoted_string string = "\"" +++ escape_specials 0 string +++ "\""
where
	escape_specials i string
		| i>=size string
			= string
		| string.[i]=='\\'
			= escape_specials (i+2) (string % (0,i-1)+++"\\"+++string % (i,dec (size string)))
		| string.[i]=='"'
			= escape_specials (i+2) (string % (0,i-1)+++"\\"+++string % (i,dec (size string)))
			= escape_specials (inc i) string

CodeGen	::	!String !Bool !(WindowFun *(PSt .l)) !CodeGenerateAsmOrCode !Pathname !Bool
			!CodeGenOptions !Processor !ApplicationOptions !Pathname !CompilerProcessIds !*(PSt .l)
			-> (!Pathname,!Bool,!CompilerProcessIds,!*(PSt .l))
CodeGen cgen` used_compiler_process_ids wf genAsmOrCode path timeprofile cgo tp ao startupdir compiler_process_ids ps
	# (cgen_ok,cgen,cgendir)		= mangleGenerator cgen` startupdir
	| not cgen_ok
		# ps				= wf [cgen] ps
		= ("",False,compiler_process_ids,ps)

	#	objpath				= MakeObjSystemPathname tp path
		path_without_suffix	= RemoveSuffix path
		command				= cgen +++ MakeCodeGenOptionsString genAsmOrCode timeprofile cgo tp
//								+++ " " +++ (watcom_quoted_string path_without_suffix)
								+++ " " +++ (quoted_string path_without_suffix)

  		errorsfilename		= tooltempdir +++ toString dirseparator +++ "errors"
		(didit,exit_code,_)	= trace_n errorsfilename CallProcess command [] cgendir "" "" errorsfilename 99
	| not didit
		= (objpath,False,compiler_process_ids,wf [  "Error: Unable to run code generator: "+++cgen
//													, command
//													, startupdir
													] ps
		)
	#	((_, errors_not_empty, error_text),ps)	= accFiles (ReadErrorsAndWarnings errorsfilename) ps
		ps										= (if errors_not_empty 
													(wf (StrictListToList error_text)) 
													( if (exit_code <> 0)
														(wf ["Error: Code generator failed for '" +++ path +++ "' with exit code: "+++toString exit_code,(quoted_string path_without_suffix)])
														id
													)
												  ) ps
	=  (objpath,exit_code==0,compiler_process_ids,ps)

mangleGenerator cgen` startupdir
	# (cgen`,opts)			= splitOptions cgen`
	# (shortOK,cgen)		= GetShortPathName (startupdir +++ "\\" +++ cgen` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ "\\" +++ cgen`) +++ "'."
		= (False,line,"")
	# cgencom = cgen % (0, size cgen - 2) +++ opts
	# cgendir = RemoveFilename (cgen % (0, size cgen - 2))
	= (True,cgencom,cgendir)

MakeCodeGenOptionsString genAsmOrCode timeprofile {ci,cs} tp
	= checkindex+++checkstack+++genasm
where
	checkindex	| ci = " -ci"; = ""
	checkstack	| cs = " -os"; = ""
	genasm		| genAsmOrCode == AsmGeneration
										= " -a"
										= ""
	
/* Links the given file:
*/

Link ::	!String !(WindowFun *(PSt .l)) !Pathname !ApplicationOptions
		!Pathname !(List Pathname) !(List Pathname) !(List Pathname) !Bool !Bool !Bool !Bool !String
		!Bool !String !Pathname !String !Processor !Bool !*(PSt .l)
		 -> (!*(PSt .l),!Bool)
Link linker` winfun path
		applicationOptions=:{ss,hs,initial_heap_size,profiling,heap_size_multiple,o,memoryProfilingMinimumHeapSize=minheap}
		optionspathname library_file_names object_file_names static_libraries static gen_relocs gen_linkmap
		link_resources resource_path gen_dll dll_syms startupdir dynlstr _ use_64_bit_processor ps
	# (ok,linker,linkerdir)				= mangleLinker linker` startupdir
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
		DoesntExist		-> createDirectory pd_optdirpath ps
		err  			-> (err,ps)
	| err <> NoDirError
		= (winfun ["Linker error: Unable to access or create: "+++optdirpath] ps,False)
	# (options_file_ok,ps)		= accFiles (write_options_file optionspathname flags hs ss initial_heap_size heap_size_multiple minheap use_64_bit_processor) ps
	| not options_file_ok
		= (winfun ["Linker error: Could not write the options object file: "+++optionspathname] ps,False)
	# linkopts =
		{ exe_path					= path
		, res_path					= resource_path
		, open_console				= o <> NoConsole
		, static_link				= static
		, gen_relocs				= gen_relocs
		, gen_linkmap				= gen_linkmap
		, link_resources			= link_resources
		, object_paths				= optionspathname :! (RemoveDup object_file_names)
	  	, dynamic_libs				= RemoveDup library_file_names
	  	, static_libs				= RemoveDup static_libraries
	  	, stack_size				= ss
		, gen_dll					= gen_dll
		, dll_names					= dll_syms
		, dynamics_path				= startupdir +++. {dirseparator} +++. dynlstr
	  	}
	# linkerpath					= RemoveFilename linker
//	# linkoptspath					= MakeFullPathname linkerpath "linkopts"
//	# linkerrspath					= MakeFullPathname linkerpath "linkerrs"
	# linkoptspath					= MakeFullPathname tooltempdir "linkopts"
	# linkerrspath					= MakeFullPathname tooltempdir "linkerrs"
	# (err,ps)						= accFiles (WriteLinkOpts linkoptspath linkopts) ps
	| isJust err
		= (winfun (fromJust err) ps,False)

	# linker = linker +++ " -I " +++ quoted_string linkoptspath +++ " -O " +++ quoted_string linkerrspath
	
	# (didit,exit_code,ost) = CallProcess linker [] linkerdir "" "" "" 99
	# diditall = if (ost == 99) didit didit
	| not diditall
		=	(winfun ["Error: Unable to run linker: "+++linker] ps, False)
	# link_ok = (exit_code==0) && (ost == 99)
	# ((err,link_errors),ps) = accFiles (ReadLinkErrors linkerrspath) ps
	| isJust err
		= (winfun (fromJust err) ps,False)
	# (errtext,errlines) = (link_errors, length link_errors)
	| errlines<>0
		= (winfun errtext ps,link_ok)
	=  (ps,link_ok)

mangleLinker linkstr` startupdir
	# (linkstr`,opts)		= splitOptions linkstr`
	# (shortOK,linkstr)		= GetShortPathName (startupdir +++ toString dirseparator +++ linkstr` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ toString dirseparator +++ linkstr`) +++ "'."
		= (False,line,"")
	# linkcom = linkstr % (0, size linkstr - 2) +++ opts
	# linkdir = RemoveFilename (linkstr % (0, size linkstr - 2))
	= (True,linkcom,linkdir)

splitOptions str
	| first_q >= len_str	= (str,"")
	= (first_str,last_str)
where
	first_str =  str%(0,dec first_q)
	last_str = str % (inc first_q, len_str)
	len_str = size str
	first_q			= FindQuoteChar str len_str 0
	FindQuoteChar str len pos	= FindChar ':' str len pos;
	FindChar	:: !Char !.String !.Int !Int -> Int;
	FindChar c line linelen pos
		| pos >= linelen		=  pos;
		| c ==  line.[pos]		=  pos;
								=  FindChar c line linelen (inc pos);
/*
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
*/
	
DynLink :: !String !String !String !*(PSt .l) -> (Bool,*PSt .l)
DynLink linker prj_path startupdir ps
	# os = 42
	# app = quoted_string linker +++ " /s " +++ quoted_string(prj_path)
	#! (ok,os) = winLaunchApp2 app startupdir False os
	= (ok && os == 42,ps)

//--- EXECUTE

Execute	::	!(WindowFun *env) !Pathname !ApplicationOptions *env -> (*env, !Bool)
Execute winfun path {o} ps
	#	(didit,_) = winLaunchApp (quoted_string path) (o<>NoConsole) 99
	| didit
		= (ps,True)
		= (winfun ["Error: Could not launch the application"] ps,False)

Execute`	::	!String !*env -> (!Bool,!Int,!*env)
Execute` command ps
	#	(didit, ec, os4)	= CallProcess command [] "" "" "" "" 99
	    diditall			= if (os4 == 99) didit didit
	| diditall
		= (True,ec,ps)
		= (False,ec,ps)

//--- OTHER STUFF

CallProcess :: !String [(!String,!String)] !String !String !String !String !*OSToolbox -> (!Bool, !Int, !*OSToolbox)
CallProcess command environment directory stdin stdout stderr os 
	| size command > 0
		#	(commandptr,os)		=  winMakeCString command os
		    envstring			=  MakeEnvironmentString environment
			(envptr,os)			=  case (size envstring == 0) of
										True	-> (0, os)
										false	-> (winMakeCString envstring os)
		    (dirptr, os)		=  case (size directory == 0) of
										True	-> (0, os)
										false	-> (winMakeCString directory os)
			(inptr,  os)		=  case (size stdin  == 0) of
										True	-> (0, os)
										false	-> (winMakeCString stdin  os)
			(outptr, os)		=  case (size stdout == 0) of
										True	-> (0, os)
										false	-> (winMakeCString stdout os)
			(errptr, os)		=  case (size stderr == 0) of
										True	-> (0, os)
										false	-> (winMakeCString stderr os)
		    (success, exitcode, os) 
								=  winCallProcess commandptr envptr dirptr inptr outptr errptr os
			os					=  winReleaseCString commandptr os
			os					=  case (envptr == 0) of
										True	-> os
										false	->  (winReleaseCString envptr os)
			os					=  case (dirptr == 0) of
										True	-> os
										false	->  (winReleaseCString dirptr os)
			os					=  case (envptr == 0) of
										True	-> os
										false	-> (winReleaseCString inptr os)
			os					=  case (envptr == 0) of
										True	-> os
										false	-> (winReleaseCString outptr os)
			os					=  case (envptr == 0) of
										True	-> os
										false	-> (winReleaseCString errptr os)
		= (success, exitcode, os)
	= (False, -1, os)
where
	MakeEnvironmentString [] = ""
    MakeEnvironmentString [ (name, value):rest ] = name +++ "=" +++ value +++ "\0" +++ MakeEnvironmentString rest

// PERSISTENT STUFF

int_to_hex v
	= {hex_char i \\ i<-[0..7]};
where
		hex_char i
			# h=(v>>((7-i)<<2)) bitand 15;
			= toChar (if (h<10) (toInt '0'+h) ((toInt 'A'-10)+h));



:: CompilingInfo = NotCompiling | CompilingInfo !CompilerProcess;

:: CompilerProcess = NoCompiler | CompilerProcess !Int !Int !Int; // thread_id thread_handle process_handle

InitCompilingInfo :: *CompilingInfo
//InitCompilingInfo = NotCompiling
InitCompilingInfo = CompilingInfo NoCompiler

compile_with_cache :: String String String String *(*CompilingInfo,*env) -> (!Bool,!Int,!*(*CompilingInfo,*env))
compile_with_cache path directory startup_arguments arguments prog=:(CompilingInfo NoCompiler, ps)
//	# startup_arguments = ""
	# thread_id=get_current_thread_id;
	# begin_arguments=startup_arguments+++" -ide "+++int_to_hex thread_id;
	# (r,compiler_thread_id,compiler_thread_handle,compiler_process_handle) = start_compiler_process (path+++"\0") (directory+++"\0") (path+++" "+++begin_arguments+++"\0");
	# r = trace_n ("Start",path,directory,path+++" "+++begin_arguments) r
	| r==0
		= trace_n ("A") (False,0,prog)
	# (ok,s) = compile_with_cache2 path directory arguments compiler_thread_id compiler_thread_handle compiler_process_handle;
	| ok
		# ci = CompilingInfo (CompilerProcess compiler_thread_id compiler_thread_handle compiler_process_handle)
		= (ok,s,(ci,ps));
		= trace_n ("B") (ok,s,prog);
compile_with_cache path directory startup_arguments arguments prog=:(CompilingInfo (CompilerProcess compiler_thread_id compiler_thread_handle compiler_process_handle),ps)
	# (ok,s) = compile_with_cache2 path directory arguments compiler_thread_id compiler_thread_handle compiler_process_handle
	| ok
		= (ok,s,prog)
	= trace_n ("C") (ok,s,(CompilingInfo NoCompiler,ps))
compile_with_cache path directory startup_arguments arguments prog=:(NotCompiling,ps)
    # command = quoted_string path +++ " " +++ arguments
	# (ok,exitcode, os4) = CallProcess command [] directory "" "" "" 99
	# ok = trace_n ("NotCompiling",command) ok
	= (ok,exitcode,prog)

compile_with_cache2 :: {#.Char} {#.Char} {#.Char} Int Int Int -> (!Bool,!Int)
compile_with_cache2 path directory arguments compiler_thread_id compiler_thread_handle compiler_process_handle
	# wm_number=get_message_number
	# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("cocl "+++arguments+++"\0")
	# r = trace_n ("Compile","cocl "+++arguments) r
	| r==0
		= trace_n ("D") (False,0)
	# (r,a,s) =get_integers_from_thread_message wm_number compiler_thread_handle
	| r==0
		= trace_n ("E") (False,s)
	= (True,s)

StartCodeGenerator	::	!String !(WindowFun *(PSt .l)) !CodeGenerateAsmOrCode !Pathname !Int !Bool !CodeGenOptions !Processor !ApplicationOptions !Pathname !CompilerProcessIds !*(PSt .l) -> (!Bool,!Pathname,!CompilerProcessIds,!*(PSt .l))
StartCodeGenerator _ _ _ _ _ _ _ _ _ _ _ _ = undef

SendRepeatResult :: !Int !.a -> (!Int,!.a)
SendRepeatResult _ _ = undef

DelayEventLoop :: !.ps -> .ps
DelayEventLoop ps
	| wait_message 0==0
		= ps

wait_message :: !Int -> Int;
wait_message r = code {
	ccall WaitMessage@0 "P:V:I"
}