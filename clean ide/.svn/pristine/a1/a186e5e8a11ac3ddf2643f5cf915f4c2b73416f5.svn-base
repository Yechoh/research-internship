implementation module IdeState

import StdPSt, StdId, StdPictureDef, StdMisc, StdList, StdProcess, StdPStClass
import StdPathname
import UtilStrictLists
from PmAbcMagic import ABCCache, AC_Init
from PmProject import Project, PR_GetTarget
import PmCompilerOptions
import typewin
import PmEnvironment
import logfile
import set_return_code
//import PmDriver
import PmFileInfo

:: *General =
	{ prefs			:: !Prefs
	, project		:: !Project
	, cache			:: !*(Maybe !*ABCCache)
	, fi_cache		:: !(Maybe FileInfoCache)
	, pr_path		:: !Pathname			// proj_path
	, stup			:: !Pathname			// appl_path
	, pm_targets	:: ![Target]
	, pm_curtarg	:: !Int
	, logfile		:: !*File
	, int_ids		:: !(!Id,!Id)
	}

initGeneral :: !Bool !CompilerOptions !String !String !Project ![Target] !*File !Id !Id-> *General
initGeneral be_verb comp_opts application_path project_path project targets logfile id1 id2
	| isNothing target_index	= abort ("Unable to find project environment in available environments.\n")
	=
	{ prefs			= prefs
	, project		= project
	, cache			= Just AC_Init
	, fi_cache		= Just FI_EmptyCache
	, pr_path		= project_path
	, stup			= application_path
	, pm_targets	= targets
	, pm_curtarg	= fromJust target_index
	, logfile		= logfile
	, int_ids		= (id1,id2)
	}
where
	prefs =
		{ be_verbose			= be_verb
		, compopts				= comp_opts
		, edwinfont				= NonProportionalFontDef
		, edwintabs				= (4,True,False,True,True)
		, number_of_processes	= 1
		}
	target_name	= PR_GetTarget project
	target_index = findIndex 0 target_name targets

	findIndex x name [] = Nothing
	findIndex x name [t=:{target_name=n}:ns]
		| n == name = Just x
		= findIndex (inc x) name ns
	
instance Typer General
where
	getTypeWinInfo gen = (dummy_twi,gen)
	setTypeWinInfo twi gen = gen

:: Prefs =
	{ be_verbose			:: !Bool
	, compopts				:: !CompilerOptions
	, edwinfont				:: !FontDef
	, edwintabs				:: !(Int,Bool,Bool,Bool,Bool)	// tabsize, autotab, showtabs, showlinenos, showsyncol
	, number_of_processes	:: !Int
	}

:: ErrPrefs		= ErrPrefs
:: SrcPrefs		= SrcPrefs
:: NewlinePrefs	= NwlPrefs

getPrefs :: !*(PSt *General) -> (Prefs,*PSt *General)
getPrefs ps = ps!ls.prefs

setPrefs :: Prefs !*(PSt *General) -> *PSt *General
setPrefs prefs ps = {ps & ls.prefs = prefs}

getProject :: !*(PSt *General) -> (Project,*PSt *General)
getProject ps = ps!ls.project

setProject :: !Project !*(PSt *General) -> *PSt *General
setProject project ps = {ps & ls.project = project}

getABCCache :: !*(PSt *General) -> *(!*ABCCache,!*PSt *General)
getABCCache ps = accPLoc (\p=:{cache = Just cache}->(cache,{p & cache = Nothing})) ps

setABCCache :: !*ABCCache !*(PSt *General) -> *PSt *General
setABCCache cache ps = {ps & ls.cache = Just cache}

getFICache :: !*(PSt *General) -> (FileInfoCache,*PSt *General)
getFICache ps = accPLoc (\p=:{fi_cache = Just fi_cache}->(fi_cache,{p & fi_cache = Nothing})) ps

setFICache :: !FileInfoCache !*(PSt *General) -> *PSt *General
setFICache ac ps = appPLoc (\p->{p & fi_cache = Just ac}) ps

getPath :: !*(PSt *General) -> (!Pathname,!*PSt *General)
getPath ps = ps!ls.pr_path

setPath :: !Pathname !*(PSt *General) -> !*PSt *General
setPath path ps = {ps & ls.pr_path = path}

getStup :: !*(PSt *General) -> (!Pathname,!*PSt *General)
getStup ps = ps!ls.stup

//-- NOT YET IMPLEMENTED....

getInterrupt :: !*(PSt *General) -> (!(Id,Id),!*PSt *General)
getInterrupt ps = accPLoc (\p=:{int_ids}->(int_ids,p)) ps

getTargets :: !*(PSt *General) -> (![Target],!*PSt *General)
getTargets ps = accPLoc (\p=:{pm_targets}->(pm_targets,p)) ps

setTargets :: ![Target] !*(PSt *General) -> !*PSt *General
setTargets ts ps = appPLoc (\p->{p & pm_targets = ts}) ps

getCurrentTarget :: !*(PSt *General) -> (!Int,!*PSt *General)
getCurrentTarget ps = accPLoc (\p=:{pm_curtarg}->(pm_curtarg,p)) ps

setCurrentTarget :: !Int !*(PSt *General) -> !*PSt *General
setCurrentTarget tg ps
	= appPLoc (\p->{p & pm_curtarg = tg}) ps

getCurrentPaths :: !*(PSt *General) -> (!(List Pathname),!*PSt *General)
getCurrentPaths ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_path,ps)

getCurrentDlibs :: !*(PSt *General) -> (!(List String),!*PSt *General)
getCurrentDlibs ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_libs,ps)

getCurrentSlibs :: !*(PSt *General) -> (!(List String),!*PSt *General)
getCurrentSlibs ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_stat,ps)

getCurrentObjts :: !*(PSt *General) -> (!(List String),!*PSt *General)
getCurrentObjts ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_objs,ps)

getCurrentComp :: !*(PSt *General) -> (!String,!*PSt *General)
getCurrentComp ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_comp,ps)

getCurrentCgen :: !*(PSt *General) -> (!String,!*PSt *General)
getCurrentCgen ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_cgen,ps)

getCurrentLink :: !*(PSt *General) -> (!String,!*PSt *General)
getCurrentLink ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_link,ps)

getCurrentVers :: !*(PSt *General) -> (!Int,!*PSt *General)
getCurrentVers ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_vers,ps)

getCurrentMeth :: !*(PSt *General) -> (!CompileMethod,!*PSt *General)
getCurrentMeth ps
	# (ct,ps) = accPLoc (\p=:{pm_targets,pm_curtarg}->(pm_targets!!pm_curtarg,p)) ps
	= (ct.target_meth,ps)

writeLog :: !String !*(PSt *General) -> !*PSt *General
writeLog message ps
	= appPLoc (\ls=:{logfile} -> {ls & logfile = writeLogfile message logfile}) ps

abortLog :: !Bool !String !*(PSt *General) -> !*PSt *General
abortLog flag message ps
	# ps		= case message of
					""	-> ps
					_	-> appPLoc (\ls=:{logfile} -> {ls & logfile = writeLogfile message logfile}) ps
	# (lf,ps)	= accPLoc (\ls=:{logfile} -> (logfile,{ls & logfile = stderr})) ps
	# (ok,ps)	= closeLogfile lf ps
//	| not ok ...
	# ps = case flag of
		True	-> set_return_code_pst (-1) ps
		_		-> ps
	= closeProcess ps

