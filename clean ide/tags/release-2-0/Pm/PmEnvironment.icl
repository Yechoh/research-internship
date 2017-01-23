implementation module PmEnvironment

import StdArray, StdFunc, StdList
import UtilIO
import UtilOptions
import UtilStrictLists
import PmPath
import StdMaybe
import Platform

EnvsFileName :== "IDEEnvs"

emptyTargets
//	= [t_StdEnv13,t_StdIO13]	// 1.3
	= [t_StdEnv20,t_StdIO20]	// 2.0

t_StdEnv :: !Target
t_StdEnv
//	= t_StdEnv13				// 1.3
	= t_StdEnv20				// 2.0

:: Target =
	{ target_name	:: !String		// environment identifier
	, target_path	:: !List String	// search paths
	, target_libs	:: !List String	// dynamic libraries
	, target_objs	:: !List String	// object files
	, target_stat	:: !List String	// static libraries
	, target_comp	:: !String		// compiler
	, target_cgen	:: !String		// code generator
	, target_link	:: !String		// static/eager linker
	, target_dynl	:: !String		// dynamic linker
	, target_vers	:: !Int			// abc version
	, target_redc	:: !Bool		// redirect console?
	, target_meth	:: !CompileMethod	// compile strategy
	}

:: CompileMethod
	= CompileSync
	| CompileAsync !Int
	| CompilePers
	
openEnvironments :: !String !String !*env -> *([Target],*env) | FileEnv env
openEnvironments stup envpath env
//	# (stup,env)				= accFiles GetFullApplicationPath env
	# ((targets,ok,_),env)		= accFiles (openEnvironments envpath) env
	| not ok
		#	targets				= emptyTargets
		#	(_,env)				= saveEnvironments envpath targets env
		#	targets				= map (fixAppPaths stup) targets
		= (targets,env)
	# targets				= map (fixAppPaths stup) targets
	= (targets,env)
where
	openEnvironments :: !String *a -> *(([Target],.Bool,{#Char}),*a) | FileSystem a
	openEnvironments envpath env
		# (opened, file, env)		= fopen envpath FReadData env
		| not opened
			= (([],False,"The file \"" +++  envpath +++ "\" could not be opened."),env)
		# (version, file)			= ReadVersion file
		| version <> EnvFileVersion
			# (_, env)				= fclose file env
			= (([],False,"The file \"" +++  envpath +++ "\" has the wrong version."+++version+++"<<<"),env)
		#! 	(options, file)			= ReadOptionsFile file
			targets					= REO options
			(closed, env)			= fclose file env
		| not closed
			=	((targets, True,"The file \"" +++ envpath +++ "\" clould not be closed."), env)	// warning genereren of zo?
		=	((targets, True,""), env)

saveEnvironments :: !String ![Target] !*env -> *(Bool,*env) | FileEnv env
saveEnvironments envpath targets env
	# (stup,env)	= accFiles GetFullApplicationPath env
	# targets		= map (unfixAppPaths stup) targets
	# (err,env)		= accFiles (saveEnvironments envpath targets) env
	# ok			= isNothing err
	= (ok,env)
where
	saveEnvironments :: !String [.Target] *a -> *(Maybe .[{#Char}],*a) | FileSystem a
	saveEnvironments envpath targets env
		# (opened, file, env)		= fopen envpath FWriteText env
		| not opened
			=	(Just ["Fatal open environments..."],env)
		#! options					= WEO targets
		#! file						= WriteOptionsFile EnvFileVersion options file
		# (closed,env)				= fclose file env
		| not closed
			= (Just ["Fatal close environments..."],env)
		= (Nothing,env)

/* Variant die in dir zoekt naar alle *.env bestanden?
 * Eerst beginnen met targets in leesbare variant weg te schrijven...
 * Rekening houden met exemplaren oude variant...
 */

EnvFileVersion :== "1.0"
emptyTarget =
	{ target_name	= ""
	, target_path	= Nil
	, target_libs	= Nil
	, target_objs	= Nil
	, target_stat	= Nil
	, target_comp	= ""
	, target_cgen	= ""
	, target_link	= ""
	, target_dynl	= ""
	, target_vers	= 42
	, target_redc	= False
	, target_meth	= CompileSync
	}

WEO prefs
	= PutOptions TargetsTable prefs

REO options
	= GetOptions TargetsTable options []

TargetsTable =
	{ ListOption "Environments" (TargetTableOption) emptyTarget (\a->ListToStrictList a) (\v a->StrictListToList v)
	}

TargetTableOption = GroupedOption "Environment" TargetTable id const

TargetTable :: OptionsTable Target
TargetTable =
	{ SimpleOption	"EnvironmentName"			(\a->a.target_name) (\v a->{a & target_name=v})
	, ListOption	"EnvironmentPaths"			(PathOption) "" (\a-> a.target_path) (\v a->{a & target_path= v})
	, ListOption	"EnvironmentDynamicLibs"	(PathOption) "" (\a-> a.target_libs) (\v a->{a & target_libs= v})
	, ListOption	"EnvironmentObjects"		(PathOption) "" (\a-> a.target_objs) (\v a->{a & target_objs= v})
	, ListOption	"EnvironmentStaticLibs"		(PathOption) "" (\a-> a.target_stat) (\v a->{a & target_stat= v})
	, SimpleOption	"EnvironmentCompiler"		(\a->a.target_comp) (\v a->{a & target_comp=v})
	, SimpleOption	"EnvironmentCodeGen"		(\a->a.target_cgen) (\v a->{a & target_cgen=v})
	, SimpleOption	"EnvironmentLinker"			(\a->a.target_link) (\v a->{a & target_link=v})
	, SimpleOption	"EnvironmentDynLink"		(\a->a.target_dynl) (\v a->{a & target_dynl=v})
	, SimpleOption	"EnvironmentVersion"		(\a->toString a.target_vers) (\v a->{a & target_vers=toInt v})
	, SimpleOption	"EnvironmentRedirect"		(\a->b2s a.target_redc) (\v a->{a & target_redc=s2b v})
	, SimpleOption	"EnvironmentCompileMethod"	(\a->m2s a.target_meth) (\v a->{a & target_meth=s2m v})
	}
where
	b2s True	= "True"
	b2s _		= "False"
	s2b "True"	= True
	s2b _		= False
	
	m2s CompileSync			= "Sync"
	m2s CompilePers			= "Pers"
	m2s (CompileAsync n)	= toString n
	
	s2m "Sync"	= CompileSync
	s2m "Pers"	= CompilePers
	s2m n		= CompileAsync (toInt n)
	
PathOption = SimpleOption "Path" id const


//---

t_StdEnv13 :: !Target
t_StdEnv13 =
	{ target_name	= "StdEnv"
	, target_path	=
		( "{Application}\\Libraries\\StdEnv"
		:! Nil
		)
	, target_libs	= PlatformDependant
//		( "{Application}\\StdEnv\\Clean System Files\\user_library"
//		:! "{Application}\\StdEnv\\Clean System Files\\gdi_library"
//		:! "{Application}\\StdEnv\\Clean System Files\\comdlg_library"
//		:! Nil
		( Nil
		)		// Win
		( Nil )	// Mac
	, target_objs	= Nil
	, target_stat	= Nil
	, target_comp	= "cocl.exe"
	, target_cgen	= "cg.exe"
	, target_link	= "StaticLinker.exe"
	, target_dynl	= "DynamicLinker.exe"
	, target_vers	= 918
	, target_redc	= False
	, target_meth	= CompileSync
	}

t_StdIO13 =
	{ target_name	= "IO 0.8.2"
	, target_path	=
		( "{Application}\\StdEnv"
		:! "{Application}\\IOInterface 0.8.2"
		:! Nil
		)
	, target_libs	= PlatformDependant
//		( "{Application}\\StdEnv\\Clean System Files\\user_library"
//		:! "{Application}\\StdEnv\\Clean System Files\\gdi_library"
//		:! "{Application}\\StdEnv\\Clean System Files\\comdlg_library"
//		:! Nil
		( Nil
		)		// Win
		( Nil )	// Mac
	, target_objs	= Nil
	, target_stat	= Nil
	, target_comp	= "cocl.exe"
	, target_cgen	= "cg.exe"
	, target_link	= "StaticLinker.exe"
	, target_dynl	= "DynamicLinker.exe"
	, target_vers	= 918
	, target_redc	= False
	, target_meth	= CompileSync
	}

t_StdEnv20 :: !Target
t_StdEnv20 =
	{ target_name	= "StdEnv"
	, target_path	=
		( "{Application}\\Libraries\\StdEnv 2.0"
		:! Nil
		)
	, target_libs	= PlatformDependant
		( Nil )	// Win
		( Nil )	// Mac
	, target_objs	= Nil
	, target_stat	= Nil
	, target_comp	= "Tools\\Clean System 2.0.0\\CleanCompiler200.exe"
	, target_cgen	= "Tools\\Clean System 2.0.0\\CodeGenerator200.exe"
	, target_link	= "Tools\\Clean System 2.0.0\\StaticLinker11.exe"
	, target_dynl	= "Tools\\Clean System 2.0.0\\DynamicLinker11.exe"
	, target_vers	= 916
	, target_redc	= False
	, target_meth	= CompilePers
	}

t_StdIO20 =
	{ t_StdEnv20
	& target_name	= "Object IO"
	, target_path	=
		(	"{Application}\\Libraries\\StdEnv 2.0"
		:!	"{Application}\\Libraries\\StdLib 2.0"
		:!	"{Application}\\Libraries\\ObjectIO 1.2.2"
		:!	"{Application}\\Libraries\\ObjectIO 1.2.2\\OS Windows"
		:!	Nil
		)
	}


//--

fixAppPaths stup target=:{target_path = path, target_libs = libs, target_objs=objs, target_stat=stat}
	= {target & target_path = path`, target_libs = libs`, target_objs=objs`, target_stat = stat`}
where
	path` = fulAppPaths stup path
	libs` = fulAppPaths stup libs
	objs` = fulAppPaths stup objs
	stat` = fulAppPaths stup stat

unfixAppPaths stup target=:{target_path = path, target_libs = libs, target_objs=objs, target_stat=stat}
	= {target & target_path = path`, target_libs = libs`, target_objs=objs`, target_stat=stat`}
where
	path` = symAppPaths stup path
	libs` = symAppPaths stup libs
	objs` = symAppPaths stup objs
	stat` = symAppPaths stup stat
	