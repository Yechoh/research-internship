definition module PmEnvironment

import StdFile,StdString,StdMaybe
import UtilStrictLists

EnvsFileName :== "IDEEnvs"

:: Target =
	{ target_name	:: !String			// environment identifier
	, target_path	:: !List String		// search paths
	, target_libs	:: !List String		// dynamic libraries
	, target_objs	:: !List String		// object files
	, target_stat	:: !List String		// static libraries
	, target_comp	:: !String			// compiler
	, target_cgen	:: !String			// code generator
	, target_link	:: !String			// static/eager linker
	, target_dynl	:: !String			// dynamic linker
	, target_vers	:: !Int				// abc version
	, target_redc	:: !Bool			// redirect console?
	, target_meth	:: !CompileMethod	// compile strategy
	}

:: CompileMethod
	= CompileSync
	| CompileAsync !Int
	| CompilePers
	
openEnvironments	:: !String !String !*env -> *([Target],*env) | FileEnv env
saveEnvironments	:: !String ![Target] !*env -> *(Bool,*env) | FileEnv env

t_StdEnv :: !Target
