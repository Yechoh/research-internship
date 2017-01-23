definition module PmFileInfo

import StdFile
import StdPathname
import UtilDate, UtilStrictLists
from PmTypes	import :: Processor, :: Modulename, :: StaticLibInfo
from PmAbcMagic	import :: ABCCache, :: ABCOptions

:: FileInfoCache

:: FileInfo =
	 {	path		:: !Pathname
	 ,	abcpath		:: !Pathname
	 ,	objpath		:: !Pathname
	 ,	sys			:: !Bool			// system file?
	 ,	seq_stack	:: !Bool			// sequential code & stack info?
	 ,	version		:: !Int
	 ,	abcOptions	:: !ABCOptions
	 ,	dcldate		:: !DATE
	 ,	icldate		:: !DATE
	 ,	abcdate		:: !DATE
	 ,	objdate		:: !DATE
	 }

FI_EmptyCache		:: !FileInfoCache
FI_GetFileInfo		:: !Processor !Pathname !ABCCache !FileInfoCache !*env -> ((!ABCCache,!FileInfoCache, !FileInfo),!*env) | FileEnv env
FI_UpdateAbcDate	:: !Pathname !Pathname !Bool !FileInfoCache !*Files -> ((!DATE,!FileInfoCache), !*Files)
FI_UpdateABCInfo	:: !Pathname !Pathname !ABCCache !FileInfoCache !*Files -> ((!DATE,!ABCCache,!FileInfoCache), !*Files)
FI_UpdateObjDate	:: !Pathname !Pathname !FileInfoCache !*Files -> (!FileInfoCache, !*Files)
FI_UpdateFileInfo	:: !Pathname !(FileInfo -> FileInfo) !FileInfoCache -> FileInfoCache
FI_GetCleanModules	:: !Pathname !StaticLibInfo !FileInfoCache -> (!List Pathname, !FileInfoCache)

YoungestObj			:: !DATE !FileInfoCache -> !DATE
