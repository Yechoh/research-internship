implementation module PmDriver

import StdArray, StdBool, StdList, StdMisc, StdEnum
import StdPSt, StdSystem, StdPStClass, StdTimer
import StdWindow

import UtilNewlinesFile, UtilIO

import IdeState

from typewin import updateTypeWindow, tw_safe_close, class Typer, :: TypeWinInfo
from typeatt import typeWinKeyboard, typeWinMouse
from errwin  import updateErrorWindow, ew_safe_close
from messwin import showInfo, :: Info(..)
from projwin import pm_update_project_window

import PmCleanSystem
import PmPath
import PmProject
from PmDialogues import doPathsDialog
import PmAbcMagic
import PmFileInfo
import PmDirCache

import interrupt,Platform
from Directory import :: DateTime(..), :: Date`(..), :: Time`(..)

//from dodebug import trace_n`
trace_n _ g :== g

//--

verboseInfo verbose info ps :== verbi verbose info ps
where
	verbi verbose info ps
		| not verbose && level3 info
			= ps
			= showInfo info ps

	level3 (Level3 _) = True
	level3 _ = False

traceInfo _ ps :== ps
//traceInfo i ps :== showInfo i ps

//--

getFICache` ps
	# (_,ps)	= getFICache ps
	# fi		= FI_EmptyCache
	= (fi,ps)


/*--- TO DO:

system module dependancy analysis is possible...

	ie. when you encounter a system module that must be recompiled then check in done list
	and remove those that depend on this system module and put them back into the todo list

should also be possible to detect cycles... -> generate warning dialogue...

---*/

System			:== "_system"

//--- project manager routines

:: SetMadeProjectFun :== (Bool -> Bool -> Project -> (PSt General) -> PSt General)

//	Compile /Check Syntax of the designated module
CompileProjectModule ::	!CompileOrCheckSyntax !Pathname !Project !SetMadeProjectFun !*(PSt General) -> *(PSt General)
CompileProjectModule compilerOrCheckSyntax path project setproject ps
	#! (_,ps)				= ClearCompilerCache ps
	# (syspaths,ps)			= getCurrentPaths ps
	# prjpaths				= PR_GetPaths project
	# srcpaths				= AppendLists prjpaths syspaths
	#! (abccache,ps)		= getABCCache ps
	#! (fileinfo,ps)		= getFICache` ps
	#! ((errs,warns,dircache),ps)
							= accFiles (DC_Setup srcpaths) ps
	# ({be_verbose},ps)		= getPrefs ps
	#! ps					= HandleDCErrors be_verbose errs warns ps
	#! (ps,fileinfo,_,abccache,project,ok,newpaths,_,_)
							= CompileTheProjectModule True compilerOrCheckSyntax path fileinfo dircache abccache project ps
	# ps					= setABCCache abccache ps
	# ps					= setFICache fileinfo ps
	= setproject ok newpaths project ps

GenAsmProjectModule :: !.Pathname !Project !SetMadeProjectFun !*(PSt General) -> *(PSt *General)
GenAsmProjectModule path project setproject ps
	#! (_,ps)				= ClearCompilerCache ps
	# (syspaths,ps)			= getCurrentPaths ps
	# prjpaths				= PR_GetPaths project
	# srcpaths				= AppendLists prjpaths syspaths
	# (abccache,ps)			= getABCCache ps
	#! (fileinfo,ps)		= getFICache` ps
	# ((errs,warns,dircache),ps)
							= accFiles (DC_Setup srcpaths) ps
	# ({be_verbose},ps)		= getPrefs ps
	# ps					= HandleDCErrors be_verbose errs warns ps
	# (ps,fileinfo,_,abccache,project,ok,newpaths,abcpath,_)
							= CompileTheProjectModule True Compilation path fileinfo dircache abccache project ps
	| not ok || newpaths
		# ps				= setABCCache abccache ps
		# ps				= setFICache fileinfo ps
		= setproject True False project ps
	#	(ps,abccache,fileinfo,project,ok,_)
							= GenCodeTheProjectModule True False AsmGeneration abcpath abccache fileinfo project ps
	# ps					= setABCCache abccache ps
	# ps					= setFICache fileinfo ps
	= setproject True ok project ps

:: CleanupCont :== !Pathname !Bool !Bool !*(PSt *General) -> *(PSt *General)

BringProjectUptoDate :: !Bool CleanupCont !*(PSt *General) -> *PSt *General
BringProjectUptoDate force continuation ps
	#  (project,ps)		= getProject ps

	#	ps				= ew_safe_close ps							// close error window
	#	ps				= tw_safe_close ps							// close types window

	#	ps				= PrecompileFase project ps

	#	ps				= showInfo (Level1 "Bring up to date...") ps
	#! (_,ps)			= ClearCompilerCache ps

	#!	(intr_info,ps) 	= getInterrupt ps
		(interact,ps) 	= getInteract ps
		ini_step		= DInit force project cleanup
	#	ps				= StartIntr intr_info ini_step step ps
	= ps
where
	PrecompileFase project ps
		# (precompile,project)	= PR_GetPrecompile project
		| isJust precompile
			# ps				= showInfo (Level1 "Precompile...") ps
			# (ok,ec,ps)		= Execute` (fromJust precompile) ps
			// error handling???
			= trace_n ("PRE",ok,ec) ps
		= trace_n "NO-PRE" ps
	
	PostlinkFase project ps
		# (postlink,project)	= PR_GetPostlink project
		| isJust postlink
			# ps				= showInfo (Level1 "Postlink...") ps
			# (ok,ec,ps)		= Execute` (fromJust postlink) ps
			= ps
		= ps
	
	cleanup :: !Bool !Bool !Bool !FileInfoCache !StaticLibInfo !(List Modulename) !Project !Bool (!*ABCCache,!(PSt *General)) -> *(!*DriverState,!PSt *General)
	cleanup ok newpaths linked fileinfo libsinfo modpaths project intr (abccache,ps)
		| newpaths && not intr		// if paths have changed -> try again
			# ps			= showInfo (Level1 "Paths have changed: remaking.") ps
			# (_,ps)		= ClearCompilerCache ps
			= MakeTheProject False fileinfo libsinfo abccache project cleanup` ps

		# ps				= PostlinkFase project ps
		
		# ps				= showInfo (Level1 "Finished making.") ps
		# ps				= setProject project ps
		# ps				= setABCCache abccache ps
		# ps				= setFICache fileinfo ps
		# ps				= pm_update_project_window ps
		# path				= PR_GetExecPath project
		= stop (DDone,continuation path linked ok ps)
	
	cleanup` :: MTPContinuation
	cleanup` = cleanup

//-- Private stuff

:: MTPContinuation :== !Bool !Bool !Bool !FileInfoCache !StaticLibInfo !(List Modulename) !Project !Bool *(!*ABCCache,!(PSt *General)) -> *(!*DriverState,!PSt *General)

MakeTheProject :: !Bool !FileInfoCache !StaticLibInfo !*ABCCache !Project !MTPContinuation !(PSt General) -> (!*DriverState,!PSt General)
MakeTheProject force fileinfo libsinfo abccache project continue ps
	# (syspaths,ps)			= getCurrentPaths ps
	# prjpaths				= PR_GetPaths project
	# srcpaths				= AppendLists prjpaths syspaths
	# ((errs,warns,dircache),ps)
							= accFiles (DC_Setup srcpaths) ps
	# ({be_verbose},ps)		= getPrefs ps
	# ps					= HandleDCErrors be_verbose errs warns ps
	# (root,project)		= PR_GetRootPathName project
	# root					= MakeDefPathname root	// avoid double compilation...
	# inidone				= Nil
	# (env_static_libs,ps)	= getCurrentSlibs ps
	# sfiles				= (StrictListToList(Concat (SL_Libs libsinfo) env_static_libs))
	# (err,ps)				= check_exists sfiles ps
	| isJust err
		# line				= Level3 ["Error: Unable to find static library: '" +++ fromJust err +++ "'."]
		# ps				= showInfo line ps
		= continue False False False fileinfo libsinfo Nil project False (abccache, ps)
	# ((errs,slibs),ps)		= accFiles (getLibs sfiles) ps
	| not (isEmpty errs)
		# line				= Level3 ["Error: Failed reading static libraries: '" :errs]
		# ps				= showInfo line ps
		= continue False False False fileinfo libsinfo Nil project False (abccache, ps)
	# slibs					= ListToStrictList slibs
	# libsinfo				= SL_SetDcls slibs libsinfo
	# ps					= showInfo (Level1 "Compiling...") ps
	# rest					= root :! Nil
	# (method,ps)			= getCurrentMeth ps
	# compinfo = case method of
				CompileSync			-> Sync
//				(CompileAsync cmax)	-> PlatformDependant
//										(AsyncWin cmax [])								// win
//										(Async [] cmax 0 NoUnknownFinishedProcessors)	// mac
				CompilePers			-> Pers InitCompilingInfo
	# ds = 
		{ project	= project
		, continue	= continue
		, fileinfo	= fileinfo
		, abccache	= abccache
		, libsinfo	= libsinfo
		, ok		= True
		, newpaths	= False
		, modpaths	= inidone
		}
	= step False (DComp force dircache compinfo rest ds) ps
where
	check_exists [] ps = (Nothing,ps)
	check_exists [file:rest] ps
		# (ok,ps) = accFiles (FExists file) ps
		| ok = check_exists rest ps
		= (Just file,ps)

:: CurrentlyCompiled =
	{ iclModule	:: !String
	, options	:: CompilerOptions
	, slot		:: !Int
	}

//--

cont :: !*(.a,*(PSt *General)) -> *(.a,*(PSt *General));
cont (ls,ps)
	# (intr_info,ps) 	= getInterrupt ps
	# ps				= ContIntr intr_info ps
	= (ls,ps)

stop :: !*(.a,*(PSt *General)) -> *(.a,*(PSt *General));
stop (ls,ps)
	# (intr_info,ps) 	= getInterrupt ps
	# ps				= StopIntr intr_info ps
	= (ls,ps)

step :: !Bool !*DriverState !*(PSt General) -> !(!*DriverState,!*(PSt General))
step intr (DInit force project setproject) ps
//	# ps				= showInfo (Level1 "Make the project...") ps
	# libsinfo			= PR_GetStaticLibsInfo project
	# (abccache,ps)		= getABCCache ps
	# (fileinfo,ps)		= getFICache` ps
	= MakeTheProject force fileinfo libsinfo abccache project setproject ps

step True (DComp force dircache (Pers inf) rest ds) ps
	# ds			= {ds & ok = False}
	# (paths,ds)	= ds!modpaths
	// compile phase finished: kill clean compiler
	# (_,ps)				= ExitCleanCompiler (inf,ps)
	= step True (DGene paths SyncCodeGeneration ds) ps

step True (DComp force dircache compinfo rest ds) ps
	// need async cocl shootdown as well..
	# ds			= {ds & ok = False}
	# (paths,ds)	= ds!modpaths
	= step True (DGene paths SyncCodeGeneration ds) ps

step intr (DComp force dircache Sync Nil ds) ps
	// compile phase finished: remove all modules not (indirectly) imported by main module
	# project				= PR_SetBuilt ds.modpaths ds.project	// removes unused modules
	# (modpaths,project)	= PR_GetModulenames True IclMod project
	# ds					= {ds & modpaths = modpaths, project = project}
	# ps					= showInfo (Level1 "Generating...") ps
	# (paths,ds)			= ds!modpaths
	= step intr (DGene paths SyncCodeGeneration ds) ps

step intr (DComp force dircache Sync (next :! rest) ds) ps
	// compile phase: check module 'next'
	| StringOccurs next ds.modpaths
		// if already done then skip
		= step intr (DComp force dircache Sync rest ds) ps
	# modname					= GetModuleName next
	| isProjLibraryModule modname ds.libsinfo
		// instead of testing explicitly put libmodules in done <= conflicts with other administration
		= step intr (DComp force dircache Sync rest ds) ps
	# (ps,dircache,ok,newpaths`,rest,compinfo,ds,_)
							= UpdateDependencies force next rest Sync dircache ds ps
	# ds	= {ds & newpaths = ds.newpaths || newpaths`, ok = ok}
	| not ok
		# (paths,ds)		= ds!modpaths
		= step intr (DGene paths SyncCodeGeneration ds) ps
	# ds	= {ds & modpaths = next :! ds.modpaths}
	= cont (DComp force dircache compinfo rest ds,ps)

step intr (DComp force dircache (Pers inf) Nil ds) ps
	// compile phase finished: kill clean compiler
	# (_,ps)				= ExitCleanCompiler (inf,ps)
	// compile phase finished: remove all modules not (indirectly) imported by main module
	# project				= PR_SetBuilt ds.modpaths ds.project	// removes unused modules
	# (modpaths,project)	= PR_GetModulenames True IclMod project
	# ds					= {ds & modpaths = modpaths, project = project}
	# ps					= showInfo (Level1 "Generating...") ps
//XXX	= step intr (DGene True newpaths fileinfo libsinfo modpaths modpaths abccache project setproject) ps
	# (paths,ds)			= ds!modpaths
	= step intr (DGene paths SyncCodeGeneration ds) ps

step intr (DComp force dircache compinfo=:(Pers _) (next :! rest) ds) ps
	// compile phase: check module 'next'
	# ps = trace_n ("comp step",next) ps
	| StringOccurs next ds.modpaths
		// if already done then skip
		= step intr (DComp force dircache compinfo rest ds) ps
	# modname					= GetModuleName next
	| isProjLibraryModule modname ds.libsinfo
		// instead of testing explicitly put libmodules in done <= conflicts with other administration
		= step intr (DComp force dircache Sync rest ds) ps
	# ps = trace_n ("update",next) ps
	# (ps,dircache,ok,newpaths`,rest,compinfo,ds,_)
							= UpdateDependencies force next rest compinfo dircache ds ps
	# ds	= {ds & newpaths = ds.newpaths || newpaths`, ok = ok}
	| not ok
		# (Pers inf)		= compinfo
		# (_,ps)			= trace_n "exit compiler!" ExitCleanCompiler (inf,ps)
		# (paths,ds)		= ds!modpaths
		= step intr (DGene paths SyncCodeGeneration ds) ps
	# ds	= {ds & modpaths = next :! ds.modpaths}
	= cont (DComp force dircache compinfo rest ds,ps)
/*
step intr (DComp force dircache (Async [] max_n_processes n_used_processors NoUnknownFinishedProcessors) Nil ds) ps
	// compile phase finished: remove all modules not (indirectly) imported by main module
	# project				= PR_SetBuilt ds.modpaths ds.project	// removes unused modules
	# (modpaths,project)	= PR_GetModulenames True IclMod project
	# ds					= {ds & modpaths = modpaths, project = project}
	# (os_error,ps)			= ClearCompilerCaches n_used_processors ps;
	# ps					= showInfo (Level1 "Generating...") ps
	# (paths,ds)			= ds!modpaths
	= step intr (DGene paths (ASyncCodeGeneration [] max_n_processes NoUnknownFinishedProcessors) ds) ps

step intr (DComp force dircache (AsyncWin _ []) Nil ds) ps
	// compile phase finished: remove all modules not (indirectly) imported by main module
	# project				= PR_SetBuilt ds.modpaths ds.project	// removes unused modules
	# (modpaths,project)	= PR_GetModulenames True IclMod project
	# ds					= {ds & modpaths = modpaths, project = project}
//	# (os_error,ps)			= ClearCompilerCaches n_used_processors ps;
	# ps					= showInfo (Level1 "Generating...") ps
	# (paths,ds)			= ds!modpaths
	= step intr (DGene paths SyncCodeGeneration ds) ps

step intr state=:(DComp force dircache compinfo=:(Async _ _ _ _) rest ds) ps
	# ps					= traceInfo (Level3 ["check_completed..."]) ps
	# (state, ps) = check_completed state ps
	# ps					= traceInfo (Level3 ["start_compilations..."]) ps
	# (state, ps) = start_compilations state ps
	= cont (state, ps)
	where
		check_completed :: !*DriverState !*(PSt General) -> !(!*DriverState,!*PSt General)
		check_completed state=:(DComp _ _ (Async current=:[_:_] cmax _ _) _ _)  ps
			= case (CompilePollCompleted ps) of
				(NoFinishedCompiler,ps)
					-> check_unknow_processors_are_known state ps
				(UnknownFinishedCompiler,ps)
					-> case state of
						DComp force dircache (Async current cmax n_used_processors unknown_finished_processors) todo ds
							# unknown_finished_processors = add_unknown_finished_processor unknown_finished_processors
							# state = DComp force dircache (Async current cmax n_used_processors unknown_finished_processors) todo ds
							-> check_completed state ps
				(FinishedCompiler completedSlot exitcode,ps)
					# ps = traceInfo (Level3 ["process_completed...",toString completedSlot,toString exitcode]) ps
					#! (state,ps) = process_completed completedSlot exitcode state ps
					-> check_completed state ps
		check_completed state ps
			= check_unknow_processors_are_known state ps

		check_unknow_processors_are_known (DComp force dircache (Async current cmax n_used_processors (UnknownFinishedProcessors n_unknown_finished_processors known_finished_processors)) todo ds) ps
			| n_unknown_finished_processors+length known_finished_processors>=cmax
				# state = DComp force dircache (Async current cmax n_used_processors NoUnknownFinishedProcessors) todo ds
				# (state,ps) = handle_completed_processes 0 state ps
					with
						handle_completed_processes process_n state ps
							| process_n>=cmax
								= (state,ps)
							| isMember process_n known_finished_processors
								= handle_completed_processes (process_n+1) state ps
								# (_,ps) = SendRepeatResult process_n ps
								/*
								# exitcode = 1
								# ps = traceInfo (Level3 ["process_completed...",toString process_n,toString exitcode]) ps
								#! (state,ps) = process_completed process_n exitcode state ps
								*/
								= handle_completed_processes (process_n+1) state ps								
				= (state, ps)
		check_unknow_processors_are_known state ps
			=	(state, ps)

		process_completed :: !Int !Int !*DriverState !*(PSt General) -> !(!*DriverState,!*PSt General)
		process_completed completedSlot exitcode (DComp force dircache (Async current cmax n_used_processors unknown_finished_processors) todo ds) ps
			# (completed, current)			= removeFromCurrent completedSlot current
			# unknown_finished_processors	= remove_from_unknown_finished_processors completedSlot unknown_finished_processors
			# (startupdir,ps)				= getStup ps
			  typewin						= updateTypeWindow (GetModuleName completed.iclModule) [typeWinKeyboard, typeWinMouse]
			# ccstring						= "dummy ccstring for now.."
			# (ps,abcpath,res)				= CompileHandleExitCode exitcode ccstring startupdir completedSlot updateErrorWindow typewin 
												completed.iclModule completed.options.listTypes ps // types param
			# (ps,fileinfo,dircache,abccache,project,ok,newpaths`,_,deps)
											= ProcessCompilerMsg Compilation completed.options completed.iclModule abcpath res ds.fileinfo dircache ds.abccache ds.project ps
			# ds							= {ds & newpaths = ds.newpaths || newpaths`, fileinfo = fileinfo, abccache = abccache, project = project, ok = ok}
			| ok
				# ds						= {ds & modpaths = completed.iclModule :! ds.modpaths}
				= (DComp force dircache (Async current cmax n_used_processors unknown_finished_processors) (Concat deps todo) ds, ps)
			// not ok
				# (os_error,ps) = ClearCompilerCaches n_used_processors ps;
				# (paths,ds)	= ds!modpaths
				= (DGene paths SyncCodeGeneration ds, ps)
			where
				removeFromCurrent :: Int [CurrentlyCompiled] -> (CurrentlyCompiled, [CurrentlyCompiled])
				removeFromCurrent _ []
					=	abort "driver.icl: unknown threadId"
				removeFromCurrent completedSlot [current=:{slot} : rest]
					| completedSlot == slot
						=	(current, rest)
					// otherwise
						# (completed, rest) = removeFromCurrent completedSlot rest
						= (completed, [current : rest])

		start_compilations :: !*DriverState !*(PSt General) -> (!*DriverState,!*PSt General)
		start_compilations state=:(DComp force dircache (Async current cmax n_used_processors unknown_finished_processors) (next :! rest) ds) ps
			// all threads used?
			| length current >= cmax
				= (state, ps)
			// compile phase: check module 'next'
			| StringOccurs next ds.modpaths || currently_compiled next current
				= start_compilations (DComp force dircache (Async current cmax n_used_processors unknown_finished_processors) rest ds) ps
			# modname = GetModuleName next
			| isProjLibraryModule modname ds.libsinfo
				// instead of testing explicitly put libmodules in done <= conflicts with other administration
				= (DComp force dircache (Async current cmax n_used_processors unknown_finished_processors) rest ds, ps)
			# (ps,dircache,ok,_,rest,compinfo,ds,_)
				= UpdateDependencies force next rest (Async current cmax n_used_processors unknown_finished_processors) dircache ds ps
			# ds = {ds & ok = ok}
			| not ok
				# (os_error,ps) = ClearCompilerCaches n_used_processors ps;
				#! (paths,ds)	= ds!modpaths
				= (DGene paths SyncCodeGeneration ds, ps)
			= start_compilations (DComp force dircache compinfo rest ds) ps
		start_compilations state ps
//			# ps = traceInfo (Level3 ["start_compilations no next..."]) ps
			= (state, ps)

		currently_compiled :: String [CurrentlyCompiled] -> Bool
		currently_compiled next current
			=	or [c.iclModule == next \\ c <- current]

step intr state=:(DComp force dircache compinfo=:(AsyncWin _ _) rest ds) ps
	# ps					= traceInfo (Level3 ["check_completed..."]) ps
	# (state, ps) = check_completed state ps
	# ps					= traceInfo (Level3 ["start_compilations..."]) ps
	# (state, ps) = start_compilations state ps
	= cont (state, ps)
	where
		check_completed :: !*DriverState !*(PSt General) -> !(!*DriverState,!*PSt General)
		check_completed state=:(DComp _ _ (AsyncWin cmax current=:[_:_]) _ _)  ps
			=	case (CompilePollCompleted ps) of
					(NoFinishedCompiler, ps)
						-> (state, ps)
					(FinishedCompiler completedSlot exitcode, ps)
						#! (state,ps) = process_completed completedSlot exitcode state ps
						-> check_completed state ps
					(UnknownFinishedCompiler,ps)
						-> (state, ps)	// -> doesn't occur on win
		check_completed state ps
			=	(state, ps)

		process_completed :: !Int !Int !*DriverState !*(PSt General) -> !(!*DriverState,!*PSt General)
		process_completed completedSlot exitcode (DComp force dircache (AsyncWin cmax current) todo ds) ps
			# (completed, current)	= removeFromCurrent completedSlot current
			# (startupdir,ps)		= getStup ps
			  typewin				= updateTypeWindow (GetModuleName completed.iclModule) [typeWinKeyboard, typeWinMouse]
			# ccstring				= "dummy ccstring for now.."
			# (ps,abcpath,res)		= CompileHandleExitCode exitcode ccstring startupdir completedSlot updateErrorWindow typewin 
										completed.iclModule completed.options.listTypes ps // types param
			# (ps,fileinfo,dircache,abccache,project,ok,newpaths`,_,deps)
									= ProcessCompilerMsg Compilation completed.options completed.iclModule abcpath res ds.fileinfo dircache ds.abccache ds.project ps
			# ds					= {ds & newpaths = ds.newpaths || newpaths`, fileinfo = fileinfo, abccache = abccache, project = project, ok = ok}
			| ok
				# ds						= {ds & modpaths = completed.iclModule :! ds.modpaths}
				= (DComp force dircache (AsyncWin cmax current) (Concat deps todo) ds, ps)
			// not ok
				# (paths,ds)	= ds!modpaths
				= (DGene paths SyncCodeGeneration ds, ps)
			where
				removeFromCurrent :: Int [CurrentlyCompiled] -> (CurrentlyCompiled, [CurrentlyCompiled])
				removeFromCurrent _ []
					=	abort "driver.icl: unknown threadId"
				removeFromCurrent completedSlot [current=:{slot} : rest]
					| completedSlot == slot
						=	(current, rest)
					// otherwise
						# (completed, rest) = removeFromCurrent completedSlot rest
						= (completed, [current : rest])

		start_compilations :: !*DriverState !*(PSt General) -> (!*DriverState,!*PSt General)
		start_compilations state=:(DComp force dircache (AsyncWin cmax current) (next :! rest) ds) ps
			| length current >= cmax
				= (state, ps)
		// compile phase: check module 'next'
			| StringOccurs next ds.modpaths || currently_compiled next current
				= start_compilations (DComp force dircache (AsyncWin cmax current) rest ds) ps
			# modname = GetModuleName next
			| isProjLibraryModule modname ds.libsinfo
				// instead of testing explicitly put libmodules in done <= conflicts with other administration
				= (DComp force dircache (AsyncWin cmax current) rest ds, ps)
			# (ps,dircache,ok,_,rest,compinfo,ds,_)
				= UpdateDependencies force next rest (AsyncWin cmax current) dircache ds ps
			# ds = {ds & ok = ok}
			| not ok
				#! (paths,ds)	= ds!modpaths
				= (DGene paths SyncCodeGeneration ds, ps)
			= start_compilations (DComp force dircache compinfo rest ds) ps
		start_compilations state ps
			= (state, ps)

		currently_compiled :: String [CurrentlyCompiled] -> Bool
		currently_compiled next current
			=	or [c.iclModule == next \\ c <- current]
*/
step intr (DGene Nil SyncCodeGeneration ds) ps
	#! ps	= showInfo (Level1 "Linking...") ps
	= step intr (DLink ds) ps
/*
step intr (DGene Nil (ASyncCodeGeneration [] _ NoUnknownFinishedProcessors) ds) ps
	#! ps	= showInfo (Level1 "Linking...") ps
	= step intr (DLink ds) ps
*/
step intr (DGene (path:!rest) SyncCodeGeneration ds) ps
	| not ds.ok || intr
		# ds = {ds & ok = False}
		= step intr (DLink ds) ps
	# (ps,abccache,fileinfo,gen,abcpath)	= CheckABCOutOfDate False path ds.abccache ds.fileinfo ds.project ps
	# (ps,abccache,fileinfo,project,ok,_)	= GenCodeTheProjectModule gen False CodeGeneration abcpath abccache fileinfo ds.project ps
	# ds = {ds & abccache = abccache, fileinfo = fileinfo, project = project, ok = ok}
	| not ok
		= step intr (DLink ds) ps
	= cont (DGene rest SyncCodeGeneration ds, ps)
/*
step intr (DGene paths (ASyncCodeGeneration busy_processes max_n_processes unknown_finished_processors) ds) ps
	# (ok,busy_processes,unknown_finished_processors,project,fileinfo,ps)
		= handle_finished_code_generators busy_processes unknown_finished_processors ds.project ds.fileinfo ps
		with
			handle_finished_code_generators busy_processes=:[_:_] unknown_finished_processors project fileinfo ps
				= case (CompilePollCompleted ps) of
					(NoFinishedCompiler, ps)
						-> check_unknow_processors_are_known busy_processes unknown_finished_processors project fileinfo ps
					(UnknownFinishedCompiler,ps)
						# unknown_finished_processors = add_unknown_finished_processor unknown_finished_processors
						-> (True,busy_processes,unknown_finished_processors,project,fileinfo,ps)
					(FinishedCompiler finished_cg_slot_n exit_code, ps)
//						# ps  = trace ("code generator finished "+++toString finished_cg_slot_n+++" "+++toString exit_code+++"\n") ps
//						# ps  = trace ("f "+++toString finished_cg_slot_n+++" "+++toString exit_code+++" ") ps

						# unknown_finished_processors = remove_from_unknown_finished_processors finished_cg_slot_n unknown_finished_processors
						# (abc_path,obj_path,busy_processes) = get_paths_and_remove_process_from_list busy_processes
							with
								get_paths_and_remove_process_from_list [busy_process=:(slot,abc_path,obj_path) : rest]
									| finished_cg_slot_n==slot
										= (abc_path,obj_path,rest)
										# (abc_path,obj_path,rest) = get_paths_and_remove_process_from_list rest
										= (abc_path,obj_path,[busy_process:rest])
								get_paths_and_remove_process_from_list []
									= abort "driver.icl: unknown code generator id"
						| exit_code==0
							# (fileinfo,ps) = accFiles (FI_UpdateObjDate abc_path obj_path fileinfo) ps
							# project = PR_SetCodeGenerated (GetModuleName abc_path) project
							-> handle_finished_code_generators busy_processes unknown_finished_processors project fileinfo ps
							-> (False,busy_processes,unknown_finished_processors,project,fileinfo,ps)
			handle_finished_code_generators [] unknown_finished_processors project fileinfo ps
				= check_unknow_processors_are_known [] unknown_finished_processors project fileinfo ps

			check_unknow_processors_are_known busy_processes (UnknownFinishedProcessors n_unknown_finished_processors known_finished_processors) project fileinfo ps
				| n_unknown_finished_processors+length known_finished_processors>=max_n_processes
					# (busy_processes,project,fileinfo,ps) = handle_completed_processes 0 busy_processes project fileinfo ps
						with
							handle_completed_processes process_n busy_processes project fileinfo ps
								| process_n>=max_n_processes
									= (busy_processes,project,fileinfo,ps)
								| isMember process_n known_finished_processors
									= handle_completed_processes (process_n+1) busy_processes project fileinfo ps
									# (_,ps) = SendRepeatResult process_n ps
									/*
									# unknown_finished_processors = remove_from_unknown_finished_processors finished_cg_slot_n unknown_finished_processors
									# (abc_path,obj_path,busy_processes) = get_paths_and_remove_process_from_list busy_processes
										with
											get_paths_and_remove_process_from_list [busy_process=:(slot,abc_path,obj_path) : rest]
												| process_n==slot
													= (abc_path,obj_path,rest)
													# (abc_path,obj_path,rest) = get_paths_and_remove_process_from_list rest
													= (abc_path,obj_path,[busy_process:rest])
											get_paths_and_remove_process_from_list []
												= abort "driver.icl: unknown code generator id"
									# (fileinfo,ps) = accFiles (FI_UpdateObjDate abc_path obj_path fileinfo) ps
									# project = PR_SetCodeGenerated (GetModuleName abc_path) project
									*/
									= handle_completed_processes (process_n+1) busy_processes project fileinfo ps								
					= (True,busy_processes,NoUnknownFinishedProcessors,project,fileinfo,ps)
			check_unknow_processors_are_known busy_processes unknown_finished_processors project fileinfo ps
				= (True,busy_processes,unknown_finished_processors,project,fileinfo,ps)
	# ds = {ds & fileinfo = fileinfo, project = project, ok = ds.ok && ok && not intr}
	| not ds.ok
		= cont (DGene paths (ASyncCodeGeneration busy_processes max_n_processes unknown_finished_processors) ds,ps)
	| length busy_processes>=max_n_processes || (case paths of Nil -> True ; _ -> False)
		= cont (DGene paths (ASyncCodeGeneration busy_processes max_n_processes unknown_finished_processors) ds,ps)
	# (ok,paths,busy_processes,fileinfo,abccache,ps) = start_code_generators paths busy_processes ds.fileinfo ds.abccache ps
		with
			start_code_generators paths=:(path :! rest) busy_processes fileinfo abccache ps
				| length busy_processes>=max_n_processes
					= (True,paths,busy_processes,fileinfo,abccache,ps)
				# (ps,abccache,fileinfo,gen,abc_path)
									= CheckABCOutOfDate False path abccache fileinfo project ps
				# cgo				= PR_GetCodeGenOptions project
				# ((abccache,fileinfo,info), ps)
									= FI_GetFileInfo cgo.tp abc_path abccache fileinfo ps
				| not gen
					= start_code_generators rest busy_processes fileinfo abccache ps
				# ps				= showInfo (Level2 (
													(foldl (+++) ("Generating code for "
																	+++ RemovePath abc_path) [" "+++RemovePath abc_path \\ (_,abc_path,_)<-busy_processes])
													)) ps
				# (startupdir,ps)	= getStup ps
				# (cgen,ps)			= getCurrentCgen ps
				# ao				= PR_GetApplicationOptions project
				# (prefs,ps)		= getPrefs ps
				# defaultCO			= prefs.compopts
				# modname			= GetModuleName abc_path
				# co				= case (PR_GetModuleInfo modname project) of
										Just modinfo	-> modinfo.compilerOptions
										_				-> defaultCO
				# tp				= ao.profiling && (not co.neverTimeProfile)
				# free_slot			= hd (removeMembers [0..max_n_processes-1] [slot \\ (slot,_,_) <- busy_processes])				

//				# ps  = trace ("code generator started "+++toString free_slot+++"\n") ps
//				# ps  = trace ("s "+++toString free_slot+++" ") ps

				# (res,obj_path,ps)	= StartCodeGenerator cgen updateErrorWindow CodeGeneration abc_path free_slot tp cgo ao startupdir ps
				| not res				
					= (False,rest,busy_processes,fileinfo,abccache,ps)
				# busy_processes	= [(free_slot,abc_path,obj_path):busy_processes]
				= start_code_generators rest busy_processes fileinfo abccache ps
			start_code_generators Nil busy_processes fileinfo abccache ps
				= (True,Nil,busy_processes,fileinfo,abccache,ps)
	# ds = {ds & fileinfo = fileinfo, abccache = abccache, ok = ok}
	= cont (DGene paths (ASyncCodeGeneration busy_processes max_n_processes unknown_finished_processors) ds, ps)
*/
step intr (DLink ds=:{ok, newpaths, fileinfo, libsinfo, modpaths, abccache, project, continue}) ps
	//	Check whether executable is out of date and relink it	if required.
	| intr || not ok
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	# lo						= PR_GetLinkOptions project
	# (prj_path,ps)				= getPath ps
	# (app_path,ps)				= getStup ps
	# (defpaths,ps)				= getCurrentPaths ps
	# prjpaths					= PR_GetPaths project
	# srcpaths					= AppendLists prjpaths defpaths

	// set up dircache for 'Clean System Files'
	# ((errs,warns,abcPathsCache),ps)	= accFiles (DC_Setup (Map MakeSystemPathname srcpaths)) ps
	// need to handle this differently? Now barfs on paths without Clean System File subdirs
	// maybe use variant DC_Setup which ignores nonexistent CSF-dirs...
	# ({be_verbose},ps)			= getPrefs ps
	# ps						= HandleDCErrors be_verbose errs warns ps
	# (ok,full_sys0,_,abcPathsCache) = DC_Search (MakeABCPathname System) abcPathsCache
	# full_sys					= full_sys0 +++ {dirseparator} +++ (MakeABCPathname System)
	# full_sys`					= MakeImpPathname full_sys
	
	# ao						= PR_GetApplicationOptions project
	// possibly patch _system to correct profiling settings...

	# tp						= PR_GetProcessor project
	# ((abccache,fileinfo,modinfo),ps)
								= FI_GetFileInfo tp  full_sys` abccache fileinfo ps
	
	# wantstp					= ao.profiling //&& (not co.neverTimeProfile)
	# compile					= /*mp <> info.abcOptions.abcMemoryProfile ||*/ wantstp <> modinfo.abcOptions.abcTimeProfile
	# lines						= if (be_verbose && compile)
									(Level3 ["["+++(MakeABCPathname System)+++",]: compiled with different options"])
									(Level3 [])
	# ps						= verboseInfo be_verbose lines ps
	# (version,ps)				= getCurrentVers ps
	# (patched, ps)				= accFiles (PatchSystemABC version compile full_sys /*ao.memoryProfiling*/ wantstp) ps
	| not patched
		# line					= Level3 ["Error: ["+++(MakeABCPathname System)+++",]: could not be patched."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	# ((abcdate,fileinfo), ps)	= accFiles (FI_UpdateAbcDate full_sys` full_sys wantstp fileinfo) ps
	# (ps,abccache,fileinfo,genabc,abcpath)
								// check _system module out of date
								= CheckABCOutOfDate True full_sys` abccache fileinfo project ps
	# (ps,abccache,fileinfo,project,ok,system_obj_path)
								// if out of date regenerate
								= GenCodeTheProjectModule genabc True CodeGeneration abcpath abccache fileinfo project ps
	# (sys_date, ps)			= accFiles (FModified full_sys) ps
	# sys_obj					= full_sys0 +++ {dirseparator} +++ (MakeObjPathname tp System)
	# (sys_obj_date,ps)			= accFiles (FModified sys_obj) ps
	# sys_obj_date_time			= DATEtoDateTime sys_obj_date
	# (abcPathsCache,ps)		= case genabc of
									True
										-> (DC_Update ((MakeObjPathname tp System),full_sys0,sys_obj_date_time) abcPathsCache,ps)
												// need to check if line above actually works now...
									False
										-> (abcPathsCache,ps)
	| not ok
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)

	# ((ok,_,_,_,sys_objs,sys_libs,abccache),ps)
								= accFiles (ParseABCDependencies` abcpath sys_date abccache) ps
	| not ok
		# line					= Level3 ["Error: ["+++(MakeABCPathname System)+++",]: could not be analysed."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	
	# prj_path`					= RemoveFilename prj_path
	# execpath					= PR_GetExecPath project
	# execpath					= fulPath app_path prj_path` execpath
	# ps						= showInfo (Level2 ("Linking '" +++ RemovePath execpath +++ "'")) ps
	
	// runtime objects and dynamic libs
	# stdl						= Concat sys_libs (standardStaticLibraries lo.method)
	# stdo						= Concat sys_objs (standardObjectFiles ao.stack_traces ao.profiling)
	# (stdoOk,ofiles,abcPathsCache)
								= case ao.standard_rte of
									True	-> GetPathNames stdo Nil abcPathsCache
									False	-> (True,Nil,abcPathsCache)
	# (stdlOk,lfiles,abcPathsCache)
								= case ao.standard_rte of
									True	-> GetPathNames stdl Nil abcPathsCache
									False	-> (True,Nil,abcPathsCache)
	| not stdoOk
		# line					= Level3 ["Link error: File: '" +++ (Head ofiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	| not stdlOk
		# line					= Level3 ["Link error: File: '" +++ (Head lfiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)

	// environment objects and dynamic libs
	# (deflibs,ps)				= getCurrentDlibs ps
	# (defobjs,ps)				= getCurrentObjts ps
	# ofiles					= Concat defobjs ofiles
	# lfiles					= Concat deflibs lfiles
/* NO: these are stored with full path so need different approach???
	# (defobjsOk,defobjs,abcPathsCache)
								= GetPathNames defobjs Nil abcPathsCache
	# (deflibsOk,deflibs,abcPathsCache)
								= GetPathNames deflibs Nil abcPathsCache
	| not defobjsOk
		# line					= Level3 ["Link error: File: '" +++ (Head defobjs) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo abccache modpaths project intr ps
	| not deflibsOk
		# line					= Level3 ["Link error: File: '" +++ (Head deflibs) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo abccache modpaths project intr ps
*/

	// clean modules
	# (clmodpaths,fileinfo)		= FI_GetCleanModules system_obj_path libsinfo fileinfo
	# ofiles					= Reverse2 clmodpaths ofiles

	// module imported objects and dynamic libs
	# abcLinkInfo				= PR_GetABCLinkInfo project
	# (objPathsOk,ofiles,abcPathsCache)
								= GetPathNames abcLinkInfo.linkObjFileNames ofiles abcPathsCache
	# (libPathsOk,lfiles,abcPathsCache)
								= GetPathNames abcLinkInfo.linkLibraryNames lfiles abcPathsCache
	| not objPathsOk
		# line					= Level3 ["Link error: File: '" +++ (Head ofiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	| not libPathsOk
		# line					= Level3 ["Link error: File: '" +++ (Head lfiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)

	// project objects and dynamic libs
/* full paths again...
	# (loobjsOk,ofiles,abcPathsCache)
								= GetPathNames lo.extraObjectModules ofiles abcPathsCache
	# (lolibsOk,lfiles,_)
								= GetPathNames lo.libraries lfiles abcPathsCache
*/
	# loobjsOk					= True
	# lolibsOk					= True
	# ofiles					= Concat lo.extraObjectModules ofiles
	# lfiles					= Concat lo.libraries lfiles
	
	| not loobjsOk
		# line					= Level3 ["Link error: File: '" +++ (Head ofiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)
	| not lolibsOk
		# line					= Level3 ["Link error: File: '" +++ (Head lfiles) +++ "' not found."]
		# ps					= showInfo line ps
		= continue False newpaths False fileinfo libsinfo modpaths project intr (abccache, ps)

	# (env_static_libs,ps)		= getCurrentSlibs ps
	#! sfiles					= Concat (SL_Libs libsinfo) env_static_libs		// only if really used?
	#! ofiles					= Reverse ofiles
	#! lfiles					= Reverse lfiles

//*	
	// .exe or .dat older than module.o
	// martijn also wants comparison with other libs and objs
	// !!! fails to check if console type options have changed...
	// ie. tries with old-fashioned PR_ExecUptoDate...
	// can't really fix this...
//	# (genabc,ps)				= CheckObjsOutOfDate genabc ofiles ps
//	# (genabc,ps) 				= CheckLibsOutOfDate genabc lfiles ps
	# (ood,ps)					= case lo.method of
									LM_Static  -> CheckExecOutOfDate genabc execpath fileinfo project ps
									LM_Dynamic -> CheckExecOutOfDate genabc  ((RemoveSuffix` execpath) +++. ".bat") fileinfo project ps
	| not ood
		= continue True False False fileinfo libsinfo modpaths project intr (abccache, ps)
//*/
	# static_info = 
		{ stat_mods				= symPaths app_path prj_path` modpaths
		, stat_objs				= symPaths app_path prj_path` ofiles
		, stat_slibs			= symPaths app_path prj_path` sfiles
		, stat_dlibs			= symPaths app_path prj_path` lfiles
		, stat_paths			= symPaths app_path prj_path` srcpaths
		, stat_app_path			= app_path
		, stat_prj_path			= prj_path`
		}
	# project					= setStaticInfo static_info project
	# (_,ps)					= accFiles (SaveProjectFile prj_path project app_path) ps
	# (linkstr,ps)				= getCurrentLink ps
	# (startupdir,ps)			= getStup ps
		
// want to wait with .exe out of date checks till here...
// ao ok
// lfiles ok
// ofiles ok
// sfiles ok
// so: chache .exe date with used ao and lfiles,ofiles,sfiles dates...
// can imporve now by Younger checking all objects

	# optionspath				= MakeOptionsName prj_path
	# (dynlstr,ps)				= getCurrentDynl ps
	# (ps, ok)					= Link linkstr updateErrorWindow execpath ao
									optionspath lfiles ofiles sfiles
									(lo.method == LM_Static)
									lo.generate_relocations
									lo.generate_link_map
									lo.link_resources
									(fulPath app_path prj_path` lo.resource_source)
									lo.generate_dll
									(fulPath app_path prj_path` lo.dll_export_list_name)
									startupdir dynlstr ps
	# project					= if ok (PR_SetLinked project) project
	= continue ok False ok fileinfo libsinfo modpaths project intr (abccache, ps)
where
	DATEtoDateTime :: !DATE -> DateTime
	DATEtoDateTime {DATE | yy,mm,dd,h,m,s}
		= ({year`=yy,month`=mm,day`=dd,dayNr`=0},{hours`=h,minutes`=m,seconds`=s})

step intr DDone ps
	= stop (DDone,ps)

//-- Compile Phase...

:: UnknownFinishedProcessors = NoUnknownFinishedProcessors | UnknownFinishedProcessors !Int ![Int]

add_unknown_finished_processor NoUnknownFinishedProcessors
	= UnknownFinishedProcessors 1 []
add_unknown_finished_processor (UnknownFinishedProcessors n_unknown_finished_processors known_finished_processors)
	= UnknownFinishedProcessors (n_unknown_finished_processors+1) known_finished_processors							

remove_from_unknown_finished_processors completedSlot (UnknownFinishedProcessors n_unknown_finished_processors known_finished_processors)
	| not (isMember completedSlot known_finished_processors)
		= UnknownFinishedProcessors n_unknown_finished_processors [completedSlot:known_finished_processors]
remove_from_unknown_finished_processors completedSlot unknown_finished_processors
	= unknown_finished_processors

:: *DriverCodeGenerationInfo
	= SyncCodeGeneration
//	| ASyncCodeGeneration ![(Int,String,String)] !Int !UnknownFinishedProcessors // [(busy_process_number,abc_path,obj_path)] max_n_processes unknown_finished_processors

:: *DriverState
	= DInit !Bool !Project !MTPContinuation
//	| DComp !(List String) !DriverCompilingInfo !(List String) !FileInfoCache !*DirCache !*ABCCache !StaticLibInfo !Project !Bool !MTPContinuation
	| DComp !Bool !*DirCache !DriverCompilingInfo !(List String) !DriverStateRecord
//XXX	| DGene !Bool !Bool !FileInfoCache !StaticLibInfo !(List String) !(List String) !*ABCCache !Project !MTPContinuation
//	| DGene !Bool !Bool !FileInfoCache !StaticLibInfo !(List String) !(List String) !DriverCodeGenerationInfo !*ABCCache !Project !MTPContinuation
	| DGene !(List String) !DriverCodeGenerationInfo !DriverStateRecord
//	| DLink !Bool !Bool !FileInfoCache !StaticLibInfo !(List String) !*ABCCache !Project !MTPContinuation
	| DLink !DriverStateRecord
	| DDone

:: *DriverStateRecord =
	{ project	:: !Project
	, continue	:: !MTPContinuation
	, fileinfo	:: !FileInfoCache
	, abccache	:: !*ABCCache
	, libsinfo	:: !StaticLibInfo
	, ok		:: !Bool
	, newpaths	:: !Bool
	, modpaths	:: !List String
	}

:: *DriverCompilingInfo
	= Sync
/*
	| AsyncWin	!Int ![CurrentlyCompiled]
	| Async	![CurrentlyCompiled] !Int !Int !UnknownFinishedProcessors // busy_processors max_n_processes n_used_processors unknown_finished_processors
*/
	| Pers	!*CompilingInfo

compiling_info :: !DriverCompilingInfo -> (String,DriverCompilingInfo)
/*
compiling_info info=:(AsyncWin _ current)
	=	(compiling_info_async current,info)
compiling_info info=:(Async current _ _ _)
	= (compiling_info_async current,info);
*/
compiling_info info
	= ("",info)

compiling_info_async []
	= ""
compiling_info_async current
	=	foldl (\s c -> s +++ " "+++(GetModuleName c.iclModule)) "Compiling:" current

//	Scan modified modules and update the dependencies (recompile if necessary).
UpdateDependencies :: !Bool !String !(List String) !DriverCompilingInfo !*DirCache !DriverStateRecord !*(PSt *General)
	-> (*(PSt *General),*DirCache,Bool,Bool,List String,DriverCompilingInfo,DriverStateRecord,Bool)
UpdateDependencies force next rest compinfo dircache ds ps`
	# (version,ps)				= getCurrentVers ps					// lift to DriverState
	# (cinf,compinfo)			= compiling_info compinfo
	# line						= Level2 ((if verbose ("Analyzing \'" +++  modname +++ "\'. ") ("")) +++ cinf)
	# ps						= verboseInfo verbose line ps
	# ((abccache,fileinfo,info),ps)
								= FI_GetFileInfo proc impname ds.abccache ds.fileinfo ps
	# ds						= {ds & abccache = abccache, fileinfo = fileinfo}
	# abcexists					= info.abcdate.exists
	| not abcexists
		# lines					= Level3 ["["+++modname+++".icl,]: no abc file"]
		#! ps					= showInfo lines ps
		= case compinfo of
			Sync				-> UpdateSyncDependencies rest impname co dircache ds ps
//			async=:(Async _ _ _ _)-> UpdateAsyncDependencies async rest impname co dircache ds ps
//			async=:(AsyncWin _ _)-> UpdateAsyncDependencies async rest impname co dircache ds ps
			(Pers info)			-> UpdatePersDependencies info rest impname co dircache ds ps
	| info.sys	// system module
		# wrongVersion			= info.version <> version
		| wrongVersion
		// add automatic recompilation if system module...
		// check with John what compiler flags we should pass...
			# line				= Level3 ["Error: System file: '" +++ modname +++ "' has incorrect abc version."]
			# ps				= showInfo line ps
			= (ps, dircache, False, False, rest, compinfo, ds, False)
		# wantstp				= tp && (not co.neverTimeProfile)
		# compile				= /*mp <> info.abcOptions.abcMemoryProfile ||*/ wantstp <> info.abcOptions.abcTimeProfile
		# lines					= (Level3 (if (verbose && compile) ["["+++modname+++".abc,]: System module compiled with different options"] []))
		# ps					= verboseInfo verbose lines ps
		| compile
			# abcPath			= MakeABCSystemPathname next
			# (patched, ps)		= accFiles (PatchSystemABC version True abcPath /*ao.memoryProfiling*/ wantstp) ps
			| patched
				# ((abcdate,fileinfo), ps)
								= accFiles (FI_UpdateAbcDate impname abcPath wantstp ds.fileinfo) ps
				# ds = {ds & fileinfo = fileinfo}
				# ((ok,mods,_,_,objs,libs,abccache),ps)
								= accFiles (ParseABCDependencies` info.abcpath abcdate ds.abccache) ps
				# ds = {ds & abccache = abccache}
				| not ok
					# line		= Level3 ["Error: Strange error parsing dependencies: '" +++ info.abcpath +++ "'."]
					// can actually only occur when failed to open .abc file...
					# ps		= showInfo line ps
					= (ps,dircache,False,False,rest,compinfo,ds,False)
				# (ok,paths,dircache)
								= LookupModulePaths mods dircache
				| not ok
					# line		= Level3 ["Error: '" +++ (Head paths) +++ "' not found."]
					# ps		= showInfo line ps
					= (ps,dircache,False,False,rest,compinfo,ds,False)
				#! rest			= Concat rest paths
				#! project		= PR_AddABCInfo next objs libs defaultCO defeo impeo ds.project
				   ds = {ds & project = project, modpaths = next :! ds.modpaths}
				=	(ps, dircache, True, False, rest, compinfo, ds, True)
			# line				= Level3 ["Error: System file: '" +++ modname +++ "' could not be patched."]
			# ps				= showInfo line ps
			=	(ps, dircache, False, False, rest, compinfo, ds, False)
		# ((ok,mods,_,_,objs,libs,abccache),ps)
								= accFiles (ParseABCDependencies` info.abcpath info.abcdate ds.abccache) ps
		# ds = {ds & abccache = abccache}
		| not ok
			# line				= Level3 ["Error: Strange error parsing dependencies: '" +++ info.abcpath +++ "'."]
			# ps				= showInfo line ps
			= (ps,dircache,False,False,rest,compinfo,ds,False)
		# (ok,paths,dircache)	= LookupModulePaths mods dircache
		| not ok
			# line				= Level3 ["Error: '" +++ (Head paths) +++ "' not found."]
			# ps				= showInfo line ps
			= (ps,dircache,False,False,rest,compinfo,ds,False)
		#! rest					= Concat paths rest
		#! project				= PR_AddABCInfo next objs libs defaultCO defeo impeo ds.project
		   ds = {ds & project = project, modpaths = next :! ds.modpaths}
		=	(ps, dircache, True, False, rest, compinfo, ds, False)

	// normal module
	| force
		# lines					= Level3 ["["+++modname+++".icl,]: force compile"]
		#! ps					= showInfo lines ps
		= case compinfo of
			Sync				-> UpdateSyncDependencies rest impname co dircache ds ps
//			async=:(Async _ _ _ _)-> UpdateAsyncDependencies async rest impname co dircache ds ps
//			async=:(AsyncWin _ _)-> UpdateAsyncDependencies async rest impname co dircache ds ps
			(Pers info)			-> UpdatePersDependencies info rest impname co dircache ds ps
	# ((ok,mods,xxx_md,xxx_dd,objs,libs,abccache),ps)
								= accFiles (ParseABCDependencies` info.abcpath info.abcdate ds.abccache) ps
	# ds = {ds & abccache = abccache}
	| not ok
		# line					= Level3 ["Error: Strange error parsing dependencies: '" +++ info.abcpath +++ "'."]
		# ps					= showInfo line ps
		= (ps,dircache,False,False,rest,compinfo,ds,False)
	# (okA,whyA,dircache)		= check_dependant_dates modname info.abcpath mods xxx_md xxx_dd dircache
	# (okC,whyC)				= check_module_options modname info co mp tp eod version
	
	| okA && okC
		# (ok,paths,dircache)	= LookupModulePaths mods dircache
		| not ok
			# line				= Level3 ["Error: '" +++ (Head paths) +++ "' not found."]
			# ps				= showInfo line ps
			= (ps,dircache,False,False,rest,compinfo,ds,False)
		#! rest					= Concat paths rest
		#! project				= PR_AddABCInfo next objs libs co defeo impeo ds.project
		   ds = {ds & project = project, modpaths = next :! ds.modpaths}
		= (ps,dircache,True,False,rest,compinfo,ds,False)
		
	# lines						= Level3 [if okC whyA whyC]
	#! ps						= showInfo lines ps
	= case compinfo of
		Sync				-> UpdateSyncDependencies rest impname co dircache ds ps
//		async=:(Async _ _ _ _ )-> UpdateAsyncDependencies async rest impname co dircache ds ps
//		async=:(AsyncWin _ _ )-> UpdateAsyncDependencies async rest impname co dircache ds ps
		(Pers info)			-> UpdatePersDependencies info rest impname co dircache ds ps
where
	(prefs,ps) 			= getPrefs ps`				// lift to DriverState

	project = ds.project
	proc				= PR_GetProcessor project
	ao					= PR_GetApplicationOptions project
	mp					= ao.memoryProfiling
	tp					= ao.profiling
	lo					= PR_GetLinkOptions project
	eod					= case lo.method of
							LM_Static	-> False
//							LM_Eager	-> True
							LM_Dynamic	-> True				
	verbose				= prefs.be_verbose
	defaultCO			= prefs.compopts
	eo =
		{	newlines	= HostNativeNewlineConvention}
	defeo = {pos_size = DefWindowPos_and_Size, eo = eo}
	impeo = {pos_size = DefWindowPos_and_Size, eo = eo}

	modname					= GetModuleName next
	impname					= MakeImpPathname next
	modinfo					= PR_GetModuleInfo modname project
	co						= case modinfo of
								Just modinfo	-> modinfo.compilerOptions
								_				-> defaultCO
/*
UpdateAsyncDependencies (AsyncWin cmax current) rest impname co dircache ds ps
	# free_slot = get_free_slot current
	# (compileStarted, fileinfo, dircache, abccache, ps)
		= CompileTheProjectModuleStart Compilation impname free_slot ds.fileinfo dircache ds.abccache ds.project ps
	# ds	= {ds & fileinfo = fileinfo, abccache = abccache}
	| compileStarted
		# current = [{iclModule = impname, options = co, slot = free_slot} : current]
		# cinf			= compiling_info_async current
		# ps = showInfo (Level2 cinf) ps
		# async = AsyncWin cmax current
		= (ps,dircache,True,False,rest,async,ds,True)
	// not compileStarted
		# cinf			= compiling_info_async current
		# ps = showInfo (Level2 cinf) ps
		# async = AsyncWin cmax current
		= (ps,dircache,False,False,rest,async,ds,False)
where
	get_free_slot :: [CurrentlyCompiled] -> Int
	get_free_slot current
		=	hd (removeMembers [0..] [slot \\ {slot} <- current])

UpdateAsyncDependencies (Async current cmax n_used_processors unknown_finished_processors) rest impname co dircache ds ps
	# free_slot = get_free_slot current
	# (compileStarted, fileinfo, dircache, abccache, ps)
		= CompileTheProjectModuleStart Compilation impname free_slot ds.fileinfo dircache ds.abccache ds.project ps
	# ds	= {ds & fileinfo = fileinfo, abccache = abccache}
	| compileStarted
		# n_used_processors = if (free_slot>=n_used_processors) (free_slot+1) n_used_processors
		# current = [{iclModule = impname, options = co, slot = free_slot} : current]
		# cinf = compiling_info_async current
		# ps = showInfo (Level2 cinf) ps
		# async = Async current cmax n_used_processors unknown_finished_processors
		= (ps,dircache,True,False,rest,async,ds,True)
	// not compileStarted
		# cinf = compiling_info_async current
		# ps = showInfo (Level2 cinf) ps
		# async = Async current cmax n_used_processors unknown_finished_processors
		= (ps,dircache,False,False,rest,async,ds,False)
where
	get_free_slot :: [CurrentlyCompiled] -> Int
	get_free_slot current
		=	hd (removeMembers [0..] [slot \\ {slot} <- current])
*/
UpdateSyncDependencies rest impname co dircache ds ps
	# (ps,fileinfo,dircache,abccache,project,ok,newpaths,_,deps)
							= CompileTheProjectModule True Compilation impname ds.fileinfo dircache ds.abccache ds.project ps
	# ds	= {ds & fileinfo = fileinfo, abccache = abccache, project = project}
	= (ps,dircache,ok,newpaths,Concat deps rest,Sync,ds,ok)

UpdatePersDependencies cstate rest impname co dircache ds ps
	# (cstate,(ps,fileinfo,dircache,abccache,project,ok,newpaths,_,deps))
			= PersistentCompile cstate Compilation impname ds.fileinfo dircache ds.abccache ds.project ps
	# ds	= {ds & fileinfo = fileinfo, abccache = abccache, project = project}
	= (ps,dircache,ok,newpaths,Concat deps rest,Pers cstate,ds,ok)


//	Compile the designated module.
CompileTheProjectModule :: !Bool !CompileOrCheckSyntax !Pathname !FileInfoCache !*DirCache !*ABCCache !Project !*(PSt *General)
	-> *(*PSt *General,FileInfoCache,*DirCache,*ABCCache,Project,Bool,Bool,Pathname,List Pathname)
CompileTheProjectModule outofdate compileOrCheckSyntax path fileinfo dircache abccache project ps
	| not outofdate 
		# proc					= PR_GetProcessor project
		# ((abccache,fileinfo,info), ps)
								= FI_GetFileInfo proc path abccache fileinfo ps		// so that its added to FileInfo cache...
		= (ps,fileinfo,dircache,abccache,project,True,False,info.abcpath,Nil)
	# (ok,ccstring,write_module_times,errwin,typwin,srcpaths,mp,tp,eod,co,startupdir,fileinfo,abccache,ps)
								= ShowInfoAndCompile compileOrCheckSyntax path fileinfo abccache project ps
	| not ok
		= (ps,fileinfo,dircache,abccache,project,False,False,"",Nil)
	# (ps,abcpath,res)			= Compile ccstring write_module_times errwin typwin compileOrCheckSyntax path srcpaths mp tp eod co startupdir ps
	= ProcessCompilerMsg compileOrCheckSyntax co path abcpath res fileinfo dircache abccache project ps

CompileTheProjectModuleStart :: !CompileOrCheckSyntax !Pathname !Int !FileInfoCache !*DirCache !*ABCCache !Project !*(PSt *General)
	-> *(!Bool, FileInfoCache, *DirCache, *ABCCache, PSt *General)
CompileTheProjectModuleStart compileOrCheckSyntax path slot fileinfo dircache abccache project ps
	# mn						= GetModuleName path
	# (ok,ccstring,write_module_times,errwin,_,srcpaths,mp,tp,eod,co,startupdir,fileinfo,abccache,ps)
								= CTPMcommon compileOrCheckSyntax path mn fileinfo abccache project ps
	| not ok
		= (False, fileinfo, dircache, abccache, ps)
	# (compileStarted,ps)		= CompileStartCommand ccstring write_module_times errwin compileOrCheckSyntax path srcpaths slot mp tp eod co startupdir ps
	= (compileStarted, fileinfo, dircache, abccache, ps)

PersistentCompile :: !*CompilingInfo /*!Bool*/ !CompileOrCheckSyntax !Pathname !FileInfoCache !*DirCache !*ABCCache !Project !*(PSt *General)
	-> (*CompilingInfo,*(*PSt *General,FileInfoCache,*DirCache,*ABCCache,Project,Bool,Bool,Pathname,List Pathname))
PersistentCompile cstate /*outofdate*/ compileOrCheckSyntax path fileinfo dircache abccache project ps
/*	| not outofdate 
		# proc					= PR_GetProcessor project
		# ((abccache,fileinfo,info), ps)
								= FI_GetFileInfo proc path abccache fileinfo ps		// so that its added to FileInfo cache...
		= (cstate,(ps,fileinfo,dircache,abccache,project,True,False,info.abcpath,Nil))
*/	# (ok,ccstring,write_module_times,errwin,typwin,srcpaths,mp,tp,eod,co,startupdir,fileinfo,abccache,ps)
								= ShowInfoAndCompile compileOrCheckSyntax path fileinfo abccache project ps
	| not ok
		= (cstate,(ps,fileinfo,dircache,abccache,project,False,False,"",Nil))
	# (cstate,(ps,abcpath,res))	= CompilePersistent ccstring write_module_times errwin typwin compileOrCheckSyntax path srcpaths mp tp eod co startupdir cstate ps
	= (cstate,ProcessCompilerMsg compileOrCheckSyntax co path abcpath res fileinfo dircache abccache project ps)


//ShowInfoAndCompile :: !CompileOrCheckSyntax !Pathname !FileInfoCache !*ABCCache !Project !*(PSt *General)
//	-> *(Bool, String, Bool, (!([String]) !*(PSt *General) -> *PSt *General), _, _, _, _, _, _, _, FileInfoCache, *ABCCache, PSt *General)
ShowInfoAndCompile compileOrCheckSyntax path fileinfo abccache project ps
	# mn						= GetModuleName path
	# line						= Level2 ((if (compileOrCheckSyntax == Compilation) "Compiling '" "Checking '") +++ mn +++ "'.")
	# ps						= showInfo line ps
	= CTPMcommon compileOrCheckSyntax path mn fileinfo abccache project ps

//CTPMcommon :: !CompileOrCheckSyntax !Pathname !Modulename !FileInfoCache !*ABCCache !Project !*(PSt *General)
//	-> *(Bool, String, Bool, (!([String]) !*(PSt *General) -> *PSt *General), _, _, _, _, _, _, _, FileInfoCache, *ABCCache, PSt *General)
CTPMcommon compileOrCheckSyntax path mn fileinfo abccache project ps0
//XXX	# ps						= showInfo line ps
	# (startupdir,ps)			= getStup ps
	# ({compopts},ps)			= getPrefs ps
	# defaultCO					= compopts
	# modinfo					= PR_GetModuleInfo mn project
	# co						= if (isJust modinfo) ((fromJust modinfo).compilerOptions) defaultCO

	# (ccstring,ps)			= getCurrentComp ps
	# write_module_times		= True//version == 918
	#! srcpaths					= Concat prjpaths syspaths

	= (True,ccstring,write_module_times,updateErrorWindow,typewin mn,srcpaths,mp,tp,eod,co,startupdir,fileinfo,abccache,ps)
where
	(syspaths,ps1)				= getCurrentPaths ps0
	(version,ps)				= getCurrentVers ps1
//XXX	mn						= GetModuleName path
//XXX	line					= Level2 ((if (compileOrCheckSyntax == Compilation) "Compiling '" "Checking '") +++ mn +++ "'.")
	ao							= PR_GetApplicationOptions project
	prjpaths					= PR_GetPaths project
//	proc						= PR_GetProcessor project
	typewin :: !String ![String] !*(PSt *General) -> *PSt *General
	typewin mn strings ps
		# (interact, ps)	= getInteract ps
		=	updateTypeWindow interact mn [typeWinKeyboard, typeWinMouse] strings ps
	mp							= ao.memoryProfiling
	tp							= ao.profiling
	lo							= PR_GetLinkOptions project
	eod					= case lo.method of
							LM_Static	-> False
//							LM_Eager	-> True
							LM_Dynamic	-> True				

//	ProcessCompilerMsg :: !CompilerOptions !Pathname !Pathname !CompilerMsg !(List FileInfo) !.DirCache !ABCCache !Project !*(PSt *General)
//							-> *(*PSt *General,List FileInfo,.DirCache,ABCCache,Project,Bool,Bool,Pathname,List String)
ProcessCompilerMsg compileOrCheckSyntax _ path abcpath (Patherror pathname) fileinfo dircache abccache project ps
	#	(interact, ps)		= getInteract ps
	| not interact
		= (ps,fileinfo,dircache,abccache,project,False,False,abcpath,Nil)
	#	(ps,project,new)	= NewPathsDialog (GetModuleName path) pathname project ps
	| new
		# (syspaths,ps)			= getCurrentPaths ps
		# prjpaths				= PR_GetPaths project
		# srcpaths				= AppendLists prjpaths syspaths
		# ((errs,warns,dircache),ps)
								= accFiles (DC_Setup srcpaths) ps
		# (prefs,ps)			= getPrefs ps
		# ps					= HandleDCErrors prefs.be_verbose errs warns ps
		// RWS: this compile is still blocking...
		= CompileTheProjectModule True compileOrCheckSyntax path fileinfo dircache abccache project ps
	= (ps,fileinfo,dircache,abccache,project,False,new,abcpath,Nil)
where
	NewPathsDialog :: !String !String !Project !*(PSt General) -> *(*PSt General,Project,Bool)
	NewPathsDialog module_name path project ps
		# (ap,ps)				= getStup ps
		# (pp,ps)				= getPath ps
		# (defp,ps)				= getCurrentPaths ps
		# prjpaths				= PR_GetPaths project
		# pp					= RemoveFilename pp
		# line					= "Where is '" +++ path +++ "' imported by '" +++ module_name +++ "'"
		# (backupproject,ps)	= getProject ps
		# ps					= setProject project ps
		# ps					= doPathsDialog line ap pp prjpaths (set defp) ps
		# (project,ps)			= getProject ps
		# (defpaths`,ps)		= getCurrentPaths ps
		# prjpaths`				= PR_GetPaths project
		# newpaths				=  not (EQStrings (SortStrings defp) (SortStrings defpaths`))
								|| not (EQStrings (SortStrings prjpaths) (SortStrings prjpaths`))
		# ps					= setProject backupproject ps
		= (ps,project,newpaths)
	where
		set defp ao ps
			# (prj,ps)	= getProject ps
			# prj		= PR_SetPaths False defp ao prj
			# ps		= setProject prj ps
			= ps

ProcessCompilerMsg _ _ path abcpath SyntaxError fileinfo dircache abccache project ps
	= (ps, fileinfo, dircache,abccache,project, False, False, abcpath,Nil)
	
ProcessCompilerMsg compileOrCheckSyntax co path abcpath CompilerOK fileinfo dircache abccache project ps0
	| compileOrCheckSyntax == SyntaxCheck
		= (ps,fileinfo,dircache,abccache,project,True, False,EmptyPathname,Nil)
	#! ((ok,dircache),ps)	= case version of
//								916	-> accFiles (PatchABCDates abcpath dircache) ps
								_ -> ((True,dircache),ps)
	| not ok
		# line				= Level3 ["Error: Unable to patch '" +++ abcpath +++ "'."]
		# ps				= showInfo line ps
		= (ps, fileinfo, dircache,abccache,project, False, False, abcpath,Nil)
	#	(abcdate,ps)		= accFiles (FModified abcpath) ps
		(((sys,stack,version,abcoptions),(mods,_,_,objs,libs),abccache), ps)
							= accFiles (Combined abcpath abcdate abccache) ps
		update				= \finfo	->
								{ finfo 
								& abcpath		= abcpath
								, abcdate		= abcdate
								, sys			= sys
								, seq_stack		= stack
								, version		= version
								, abcOptions	= abcoptions
								}
		fileinfo			= FI_UpdateFileInfo (MakeImpPathname path) update fileinfo


	| not ok	// fairly useless, only if unable to open abc...
		# line				= Level3 ["Error: Unable to open '" +++ abcpath +++ "'."]
		# ps				= showInfo line ps
		= (ps, fileinfo, dircache,abccache,project, False, False, abcpath,Nil)
	# (ok,paths,dircache)	= LookupModulePaths mods dircache
	| not ok				// NO, should fix with add new paths dialogue...
		# line				= Level3 ["Error: Unable to find '" +++ (Head paths) +++ "'."]
		# ps				= showInfo line ps
		= (ps, fileinfo, dircache,abccache,project, False, False, abcpath,Nil)
	#! project				= PR_AddABCInfo path objs libs co defeo impeo project
	= (ps,fileinfo,dircache,abccache,project,ok, False, abcpath, paths)
where
	(version,ps)		= getCurrentVers ps0
	eo = {	newlines	= HostNativeNewlineConvention}
	defeo = {pos_size = DefWindowPos_and_Size, eo = eo}
	impeo = {pos_size = DefWindowPos_and_Size, eo = eo}

check_dependant_dates :: !String !Pathname !(List Modulename) !(Maybe ModuleDate) !(List ModuleDate) !*DirCache -> (Bool,String,*DirCache)
check_dependant_dates modname abcpath mods xxx_md xxx_dd dircache
	| isNothing xxx_md
		= (False,"["+++modname+++".icl,]: has no date",dircache)
	# xxx_md			= fromJust xxx_md
	# (ok,_,yyy_md,dircache)		=  DC_Search (RemovePath (MakeImpPathname abcpath)) dircache
	| not ok
		= (False,"["+++modname+++".icl,]: not found in cached paths",dircache)
	| not (eqDate xxx_md yyy_md)
		= (False,"["+++modname+++".icl,]: .icl has changed",dircache)
	= moretricks mods xxx_dd dircache
where
	moretricks Nil Nil dircache = (True,"Fine!",dircache)
	moretricks Nil _ dircache = (False,"["+++modname+++".icl,]: more stored dates than modules??",dircache)
	moretricks _ Nil dircache = (False,"["+++modname+++".icl,]: more stored modules than dates??",dircache)
	moretricks (md:!ms) (dt:!ds) dc
		# (ok,_,depdate,dc) = DC_Search (MakeDefPathname md) dc
		| not ok = (False,"["+++modname+++".icl,]: can`t find "+++md+++" in cached paths",dc)
		| not (eqDate depdate dt) = (False,"["+++modname+++".icl,]: "+++md+++" has changed",dc)
		= moretricks ms ds dc
	moretricks _ _ _ = abort "driver.icl: Fooling Clean warnings"

	eqDate (ld,lt) (rd,rt)
		| lt.seconds`	<> rt.seconds`	= False
		| lt.minutes`	<> rt.minutes`	= False
		| lt.hours`		<> rt.hours`	= False
		| ld.day`		<> rd.day`		= False
		| ld.month`		<> rd.month`	= False
		| ld.year`		<> rd.year`		= False
		= True

check_module_options :: !String !.FileInfo !.CompilerOptions !.Bool !.Bool !.Bool !.Int -> !(.Bool,{#Char});
check_module_options modname info=:{version,abcOptions} co mp tp expectedEagerOrDynamic expectedVersion
	| version <> expectedVersion
		= (False,"["+++modname+++".icl,]: .abc out of date, different abc version.")

	|	abcOptions.abcTimeProfile			<> expectedTimeProfile
		= (False,"["+++modname+++".icl,]: .abc out of date, different compiler options. (Time Profiling)")
	|	abcOptions.abcMemoryProfile			<> expectedMemoryProfile		// <- is this how we use it now?
		= (False,"["+++modname+++".icl,]: .abc out of date, different compiler options. (Heap Profiling)")
	|	abcOptions.abcStrictnessAnalysis	<> expectedStrictnessAnalysis
		= (False,"["+++modname+++".icl,]: .abc out of date, different compiler options. (Strictness Analysis)")
//	||	abcOptions.abcGenerateComments		<> expectedGenerateComments	// <- do we need to regenerate for this?
	|	(expectedGenerateComments && (not abcOptions.abcGenerateComments)) // want comments but don't have
		= (False,"["+++modname+++".icl,]: .abc out of date, different compiler options. (Generate Comments)")
	|	abcOptions.abcReuseUniqueNodes		<> expectedReuseUniqueNodes
		= (False,"["+++modname+++".icl,]: .abc out of date, different compiler options. (Reuse Unique Nodes)")
	|	abcOptions.abcBeVerbose				<> expectedBeVerbose
		= (False,"["+++modname+++".icl,]: .abc out of date, different compiler options. (Verbose or Dynamic)")
	= (True,"")
where
	expectedMemoryProfile 		= expectedEagerOrDynamic || (mp && (not co.neverMemoryProfile))
	expectedTimeProfile			= tp && (not co.neverTimeProfile)
	expectedStrictnessAnalysis	= co.sa
	expectedBeVerbose			= expectedEagerOrDynamic
	expectedGenerateComments	= co.gc
	expectedReuseUniqueNodes	= co.reuseUniqueNodes

//-- Generate Phase...

//	Generate code for the designated module.
GenCodeTheProjectModule :: !.Bool !.Bool !.CodeGenerateAsmOrCode !.Pathname !*ABCCache !FileInfoCache !Project !*(PSt General) -> *(*PSt General,*ABCCache,FileInfoCache,Project,Bool,Pathname)
GenCodeTheProjectModule outofdate sys genAsmOrCode abc_path abccache fileinfo project ps`
	# ((abccache,fileinfo,info), ps)	= FI_GetFileInfo cgo.tp abc_path abccache fileinfo ps
	| not outofdate
		=	(ps,abccache,fileinfo,project,True,info.objpath)
	# ps					= showInfo (Level2 ("Generating code for '" +++ RemovePath abc_path +++ "'.")) ps
	# (startupdir,ps)		= getStup ps
	# (cgen,ps)				= getCurrentCgen ps

	# (ps, objpath,res)		= CodeGen cgen updateErrorWindow genAsmOrCode abc_path tp cgo ao startupdir ps
	| genAsmOrCode == CodeGeneration && res
		# (fileinfo,ps)		= accFiles (FI_UpdateObjDate abc_path objpath fileinfo) ps
		# project			= if sys
								(PR_SetSysCodeGenerated project)
								(PR_SetCodeGenerated mn project)
		= (ps, abccache, fileinfo, project, res, objpath)
	= (ps, abccache, fileinfo, project, res, objpath)
where
	cgo			= PR_GetCodeGenOptions project
	ao			= PR_GetApplicationOptions project
	mn			= GetModuleName abc_path

	tp			= ao.profiling && (not co.neverTimeProfile)
	modname		= mn
	modinfo		= PR_GetModuleInfo modname project
	co			= case modinfo of
					Just modinfo	-> modinfo.compilerOptions
					_				-> defaultCO
	defaultCO	= prefs.compopts
	(prefs,ps)	= getPrefs ps`
	
//	Checks whether .o files in the project are out of date.
CheckABCOutOfDate :: !.Bool !.Pathname !*ABCCache !FileInfoCache !Project !*(PSt General) -> *(*(PSt General),*ABCCache,FileInfoCache,Bool,Pathname)
CheckABCOutOfDate sys path abccache fileinfo project ps
	# tp						= PR_GetProcessor project
	# ((abccache,fileinfo,modinfo), ps)
								= FI_GetFileInfo tp path abccache fileinfo ps
	# abcexists					= modinfo.abcdate.exists
	# objexists					= modinfo.objdate.exists
	# olderdate					= Older_Date modinfo.objdate modinfo.abcdate
	# abc						= abcexists && ( olderdate || not objexists )
	# mn						= GetModuleName path
	# cg_opt					= if sys
									(not (PR_SysUptoDate project))
									(not (PR_ABCUpToDate mn project))
	# gencode					= cg_opt || abc
	# ({be_verbose},ps)			= getPrefs ps
	# lines						= if be_verbose
									(Level3 (MakeABCOutOfDateMessage mn abc abcexists objexists cg_opt))
									(Level3 [])
	# ps						= verboseInfo be_verbose lines ps
	= (ps, abccache,fileinfo, gencode, modinfo.abcpath)
where
	MakeABCOutOfDateMessage :: !Modulename !Bool !Bool !Bool !Bool -> [String]
	MakeABCOutOfDateMessage mn abc abcexists objexists cgo
		| abcexists
			| abc || not objexists
				| cgo
					| objexists
						= ["[" +++ mn +++ ".obj,]: is older than .abc file, new paths or new code generator options set"]
					= ["[" +++ mn +++ ".obj,]: does not exist, new paths or new code generator options set"]
				| objexists
					= ["[" +++ mn +++ ".obj,]: is older than .abc file"]
				= ["[" +++ mn +++ ".obj,]: does not exist"]
			| cgo
				= ["[" +++ mn +++ ".obj,]: new paths or new code generator options set"]
			= []
		= []
	
//-- Link Phase...

MakeOptionsName :: !.String -> String
MakeOptionsName path
	= path`+++{dirseparator}+++"Clean System Files"+++{dirseparator}+++"_"+++name+++"_options.o"
where
	path` = RemoveFilename path
	name  = RemoveSuffix` (RemovePath path)

CheckExecOutOfDate :: !Bool !Pathname !FileInfoCache !Project !*(PSt General) -> *(Bool,*PSt General)
CheckExecOutOfDate gen execpath fileinfo project ps
	| gen
		= (True,ps)
	#	({be_verbose},ps)	= getPrefs ps
	#	execname			= RemovePath execpath
	| not (PR_ExecUpToDate project)
		#	lines			= if be_verbose
								(Level3 ["'" +++ execname +++ "' was linked with different application options"])
								(Level3 [])
		= (True, verboseInfo be_verbose lines ps)
	#	(date, ps)			= accFiles (FModified execpath) ps
	#	youngest			= YoungestObj NoDate fileinfo
	#	link				= youngest.exists && (not date.exists || Older_Date date youngest)
	| link
		#	lines			= if be_verbose
								(if date.exists
									(Level3 ["[" +++ execname +++ ",]: is older than .obj files. Linking new executable."])
									(Level3 ["[" +++ execname +++ ",]: does not exist. Linking new executable."])
									)
								(Level3 [])
		= (True, verboseInfo be_verbose lines ps)
	= (False,ps)

//-- dircache functions

GetPathNames :: !(List String) !(List String) !*DirCache -> !(.Bool,List String,!*DirCache)
GetPathNames Nil acc cache = (True, acc, cache)
GetPathNames (fn:!fns) acc cache
	# (ok,pn,_,cache) = DC_Search fn cache
	| ok = GetPathNames fns (pn +++ {dirseparator} +++ fn :! acc) cache
	= (False, (fn :! Nil), cache)

// Lookup Module Paths in Directory Cache
LookupModulePaths :: !(List .String) !*DirCache -> (Bool,.List Pathname,*DirCache);
LookupModulePaths Nil dc
	= (True,Nil,dc)
LookupModulePaths (mn :! ms) dc
	# mn			= MakeDefPathname mn
	# (ok,pt,_,dc)	= DC_Search mn dc
	| not ok
//		# dc		= trace_n ("LookupModulePaths: couldn't find: "+++mn) dc
//		= LookupModulePaths ms dc
		= (False, mn :! Nil, dc)
	# (ok,ps,dc)	= LookupModulePaths ms dc
	| not ok
		= (ok, ps, dc)
	= (ok,MakeFullPathname pt mn :! ps, dc)
					
//LookupModulePaths` :: !(List .String) !(List .String) !*DirCache -> (Bool,.List Pathname,.List DateTime,*DirCache);
LookupModulePaths` Nil acc dc
	= (True,Nil,dc)
LookupModulePaths` (mn :! ms) acc dc
	# mn			= MakeDefPathname mn
	# (ok,pt,_,dc)	= DC_Search mn dc
	| not ok
		= (False, mn :! acc, dc)
	= LookupModulePaths` ms (MakeFullPathname pt mn :! acc) dc
					
//-- Handle DirCache Setup Errors...

HandleDCErrors :: !Bool ![String] ![Warn] !*(PSt *General) -> *(PSt *General)
HandleDCErrors _ [] [] ps
	= ps
HandleDCErrors verbose [] warns ps
	# line				= Level3 (flatten
							[[ "Warning: Multiple file instances: '" +++ n +++ "'."
							,  "First found at: '" +++ p +++ "'."
							:[ "Also found at: '" +++ p +++ "'."
							\\ (_,p,_) <- c
							]
							] \\ Warn n p c <- warns])
	= verboseInfo verbose line ps
HandleDCErrors verbose errs _ ps
	# line				= Level3 ["Warning: Unable to setup directory cache: '" +++ err +++ "'." \\ err <- errs]
	= verboseInfo verbose line ps
