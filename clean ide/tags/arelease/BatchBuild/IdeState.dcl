definition module IdeState

import StdPSt, StdId, StdPictureDef
import StdPathname
import UtilStrictLists
from PmAbcMagic import ABCCache
from PmProject import Project
import PmCompilerOptions
import typewin
import PmEnvironment
from PmFileInfo import FileInfoCache

:: *General

initGeneral :: !Bool !CompilerOptions !String !String !Project ![Target] !*File !Id !Id -> *General

instance Typer General

:: Prefs =
	{ be_verbose			:: !Bool
	, compopts				:: !CompilerOptions
	, edwinfont				:: !FontDef
	, edwintabs				:: !(Int,Bool,Bool,Bool,Bool)
	, number_of_processes	:: !Int
	}

:: ErrPrefs
:: SrcPrefs
:: NewlinePrefs

getPrefs :: !*(PSt *General) -> (Prefs,*PSt *General)
setPrefs :: Prefs !*(PSt *General) -> *PSt *General

getProject :: !*(PSt *General) -> (Project,*PSt *General)
setProject :: !Project !*(PSt *General) -> *PSt *General

getABCCache :: !*(PSt *General) -> *(!*ABCCache,!*PSt *General)
setABCCache :: !*ABCCache !*(PSt *General) -> *PSt *General

getFICache :: !*(PSt *General) -> (FileInfoCache,*PSt *General)
setFICache :: !FileInfoCache !*(PSt *General) -> *PSt *General

getPath :: !*(PSt *General) -> (!Pathname,!*PSt *General)
setPath :: !Pathname !*(PSt *General) -> !*PSt *General

getStup :: !*(PSt *General) -> (!Pathname,!*PSt *General)
getInterrupt :: !*(PSt *General) -> (!(Id,Id),!*PSt *General)
//getKeyMapping

getTargets :: !*(PSt *General) -> (![Target],!*PSt *General)
setTargets :: ![Target] !*(PSt *General) -> !*PSt *General
getCurrentTarget :: !*(PSt *General) -> (!Int,!*PSt *General)
setCurrentTarget :: !Int !*(PSt *General) -> !*PSt *General
getCurrentPaths :: !*(PSt *General) -> (!(List Pathname),!*PSt *General)
getCurrentDlibs :: !*(PSt *General) -> (!(List String),!*PSt *General)
getCurrentSlibs :: !*(PSt *General) -> (!(List String),!*PSt *General)
getCurrentObjts :: !*(PSt *General) -> (!(List String),!*PSt *General)
getCurrentComp :: !*(PSt *General) -> (!String,!*PSt *General)
getCurrentCgen :: !*(PSt *General) -> (!String,!*PSt *General)
getCurrentLink :: !*(PSt *General) -> (!String,!*PSt *General)
getCurrentVers :: !*(PSt *General) -> (!Int,!*PSt *General)
getCurrentMeth :: !*(PSt *General) -> (!CompileMethod,!*PSt *General)

writeLog :: !String !*(PSt *General) -> !*PSt *General
abortLog :: !Bool !String !*(PSt *General) -> !*PSt *General
