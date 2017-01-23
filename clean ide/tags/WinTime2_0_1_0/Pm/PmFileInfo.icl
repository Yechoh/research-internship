implementation module PmFileInfo

import StdBool
from UtilIO import FModified
from UtilDate import Older_Date
import UtilStrictLists
import PmPath, PmCompilerOptions, PmTypes
import PmAbcMagic

/*
	? switch to array for cache instead of List
	? use dircache for certain file mod datetime lookups
*/

//--- FileInfo stuff

:: FileInfo =
	 {	path		:: !Pathname
	 ,	abcpath		:: !Pathname
	 ,	objpath		:: !Pathname
	 ,	sys			:: !Bool			// system file?
	 ,	seq_stack	:: !Bool			// sequential code & stack info?
	 ,	version		:: !Int				// abc - version
	 ,	abcOptions	:: !ABCOptions
	 ,	dcldate		:: !DATE
	 ,	icldate		:: !DATE
	 ,	abcdate		:: !DATE
	 ,	objdate		:: !DATE
	 }

:: FileInfoCache :== List FileInfo

FI_EmptyCache :: FileInfoCache
FI_EmptyCache = Nil

FI_GetFileInfo :: !Processor !Pathname !ABCCache !FileInfoCache !*env -> ((!ABCCache,!FileInfoCache, !FileInfo),!*env) | FileEnv env
FI_GetFileInfo tp path abccache fileinfo ps
	= accFiles (GetFileInfo1 tp path abccache fileinfo fileinfo) ps
where
	GetFileInfo1 ::	!Processor !Pathname !ABCCache !FileInfoCache !FileInfoCache !Files
					-> ((!ABCCache,!FileInfoCache, !FileInfo), Files)
	GetFileInfo1 tp this_path abccache fileinfo=:((info=:{path,abcpath,objpath}) :! rest) acc files
		| this_path==path || this_path==abcpath || this_path==objpath
			= ((abccache,acc, info), files)
			= GetFileInfo1 tp this_path abccache rest acc files
	GetFileInfo1 tp path abccache Nil acc files
		#	dclpath				= MakeDefPathname path
		#	iclpath				= MakeImpPathname path
//		#	(abcpath, files)	= MakeABCSystemPathname path files
		#	abcpath				= MakeABCSystemPathname path
//		#	(objpath, files)	= MakeObjSystemPathname tp path files
		#	objpath				= MakeObjSystemPathname tp path
		#	(dcldate, files)	= FModified dclpath files
		#	(icldate, files)	= FModified iclpath files
		#	(abcdate, files)	= FModified abcpath files
		#	(objdate, files)	= FModified objpath files
		| not abcdate.exists
			#	finfo	= { path		= iclpath,
							abcpath		= abcpath,	objpath		= objpath,
							dcldate		= dcldate,	icldate		= icldate,
							abcdate		= abcdate,	objdate		= objdate,
							sys			= False,	seq_stack	= False,
							version		= -1,		abcOptions	= DefaultABCOptions }
			= ((abccache,finfo:!acc, finfo), files)
		// otherwise
			#	((sys,seq_stack,version,abcOptions,abccache),files)
									= GetABCCompiledInfo False abcpath abccache files
				finfo	= { path		= iclpath,
							abcpath		= abcpath,	objpath		= objpath,
							dcldate		= dcldate,	icldate		= icldate,
							abcdate		= abcdate,	objdate		= objdate,
							sys			= sys,		seq_stack	= seq_stack,
							version		= version,	abcOptions	= abcOptions }
			= ((abccache,finfo:!acc, finfo), files)

FI_UpdateFileInfo :: !Pathname !(FileInfo -> FileInfo) !FileInfoCache -> FileInfoCache
FI_UpdateFileInfo key update list = UpdateFileInfo1 key update list Nil
where
	UpdateFileInfo1 :: !Pathname !(FileInfo -> FileInfo) !FileInfoCache !FileInfoCache -> FileInfoCache
	UpdateFileInfo1 key update_function Nil acc
		= acc
	UpdateFileInfo1 key update_function ((first=:{path,abcpath,objpath}):!rest) acc
		| key == path || key == abcpath || key == objpath
			= Reverse2 (update_function first:!rest) acc
			= UpdateFileInfo1 key update_function rest (first:!acc)
						
FI_UpdateAbcDate :: !Pathname !Pathname !Bool !FileInfoCache !*Files -> ((!DATE,!FileInfoCache), !*Files)
FI_UpdateAbcDate path abcPath abcTimeProfile fileInfo files
	# (abcDate, files)		=	FModified abcPath files
	# fileInfo = FI_UpdateFileInfo path (\info -> {info & abcpath=abcPath, abcdate=abcDate, abcOptions = {info.abcOptions & abcTimeProfile = abcTimeProfile}}) fileInfo
	=	((abcDate,fileInfo), files)

FI_UpdateABCInfo :: !Pathname !Pathname !ABCCache !FileInfoCache !*Files -> ((!DATE,!ABCCache,!FileInfoCache), !*Files)
FI_UpdateABCInfo path abcPath abcCache fileInfo files
	#	(abcDate,files)							= FModified abcPath files
		((sys,stack,version,abcOptions,abcCache), files)
												= GetABCCompiledInfo True abcPath abcCache files
		update									= \finfo	->
													{ finfo 
													& abcpath		= abcPath
													, abcdate		= abcDate
													, sys			= sys
													, seq_stack		= stack
													, version		= version
													, abcOptions	= abcOptions
													}
		fileInfo								= FI_UpdateFileInfo path update fileInfo
	= ((abcDate,abcCache,fileInfo),files)

FI_UpdateObjDate :: !Pathname !Pathname !FileInfoCache !*Files -> (!FileInfoCache, !*Files)
FI_UpdateObjDate path objPath fileInfo files
	#	(objDate, files)		= FModified objPath files
		update				= \finfo ->
								{ finfo
								& objpath	= objPath
								, objdate	= objDate
								}
		fileInfo			= FI_UpdateFileInfo path update fileInfo
	= (fileInfo,files)

FI_GetCleanModules :: !Pathname !StaticLibInfo !FileInfoCache -> (!List Pathname, !FileInfoCache)
FI_GetCleanModules system_obj_path libsinfo fileinfo
	# clmodpaths				= Map (\{objpath}->objpath) fileinfo
	# clmodpaths				= Filter notSystemObject clmodpaths
	# clmodpaths				= Filter notLibraryObject clmodpaths
	# clmodpaths				= Reverse clmodpaths
	= (clmodpaths,fileinfo)
where
	notSystemObject objpath = objpath <> system_obj_path
	notLibraryObject objpath = not (isProjLibraryModule (GetModuleName objpath) libsinfo)

//	Finds the most recently modified .obj file in FileInfo...
YoungestObj :: !DATE !FileInfoCache -> DATE
YoungestObj youngest Nil = youngest
YoungestObj youngest ({objdate}:!rest)
	| not youngest.exists
		= YoungestObj objdate rest
	| not objdate.exists
		= YoungestObj youngest rest
	| Older_Date youngest objdate
		= YoungestObj objdate rest
	// otherwise
		= YoungestObj youngest rest
