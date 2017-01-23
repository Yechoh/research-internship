implementation module UtilIO

/* OS dependent module */
/* Primitives which 'should be' in the standard CLEAN IO lib */

import StdArray, StdBool, StdClass, StdFile, StdList, StdMisc
import UtilDate

import StdSystem
import ostypes, OS_utilities;
from files import GetFInfo,GetCatInfo1,GetCatInfo2/*,GetWDInfo*/,HGetVol;

//---

import StdPathname, Directory

FReadOnly :: !{#Char} !*env -> (!Bool, !*env) | FileSystem env
FReadOnly path files
	# ((ok,dir),files)	= pd_StringToPath path files
	| not ok
		= (False,files)
	# ((err,fi),files)	= getFileInfo dir files
	| err <> NoDirError
		= (False,files)
	= (fi.pi_fileInfo.isReadOnly,files)

/* old version...
FReadOnly path files
	# dir				= RemoveFilename path
	# fnm				= RemovePath path
	# ((ok,dir),files)	= pd_StringToPath dir files
	| not ok
		= (False,files)
	# ((err,dct),files)	= getDirectoryContents dir files
	| err <> NoDirError
		= (False,files)
//	# dct				= map (\{fileInfo}->fileInfo.pi_fileInfo) dct
	# dct				= filter (\{fileName}->fileName==fnm) dct
	| isEmpty dct
		= (False,files)
	= ((hd dct).fileInfo.pi_fileInfo.isReadOnly,files)
*/
//--
//import files
from PmCleanSystem import Launch

LaunchApplication :: !{#Char} !{#Char} !Bool !Files -> ( !Bool, !Files)
LaunchApplication execpath homepath console files
	// set working directory???
	# (error_n,files) = Launch execpath files
	| error_n >= 0
		= (True,files)
		= (False,files)

/*	Returns True if the file name exists.
*/

FExists	:: !String !Files -> (!Bool, !Files);
FExists name disk = (result == 0, disk);
where
	(result,_,_)	= GetFInfo name NewToolbox;


/*	Returns the last modification date of the indicated file.
*/

FModified :: !String !Files -> (!DATE, !Files);
FModified path disk
	| result <> 0	= (NoDate,disk);
					= ({exists=True,yy=years,mm=months,dd=days,h=hours,m=minutes,s=seconds},disk);
where
	(result,modification,_)		= GetFInfo path NewToolbox;
	(years,months,days,_,_)		= Secs2Date modification 0;
	(hours,minutes,seconds,_)	= Secs2Time  modification 0;


/*	Returns directory in which the indicated application resides.
*/

FStartUpDir :: !String !*Files -> (!String, !*Files);
FStartUpDir applicationname files
	| result==0
		= (pathName % (0,size pathName-2), files);
where
	(result,wd_vref_num,directory_id,tb1)	= HGetVol NewToolbox;
	(pathName,_)	= Get_directory_path wd_vref_num directory_id "" tb1;

GetFullApplicationPath :: !*Files -> ({#Char}, *Files);
GetFullApplicationPath files
	=	FStartUpDir "" files;


RemoveFileName :: !String -> String;
RemoveFileName path
	| found	= (path % (0, dec position));
			= path;
where 
	(found,position)	= LastColon path last;
	last				= dec (size path);
		
LastColon :: !String !Int -> (!Bool, !Int);
LastColon s i
	| i <= 0
		= (False,0);
	| dirseparator==s.[i]
	 	= (True, i);
		= LastColon s (dec i);

Get_name_and_parent_id_of_directory :: !Int !Int !*Toolbox -> (!String,!Int,!*Toolbox);
Get_name_and_parent_id_of_directory volumeNumber directoryId tb
	|	0 == osError	= (folderName,parentId,tb1);
						= abort ("Error code returned by BPGetCatInfo: " +++ toString osError);
where
	(osError,folderName,parentId,tb1) = GetCatInfo2 volumeNumber directoryId String64 tb;

Get_directory_path :: !Int !Int !String !*Toolbox -> (!String, !*Toolbox);
Get_directory_path volumeNumber directoryId path tb
	| directoryId==2
		= (folderName +++ ":" +++ path, tb1);
		= Get_directory_path volumeNumber parentId (folderName +++ ":" +++ path) tb1;
where
		(folderName, parentId, tb1) = Get_name_and_parent_id_of_directory volumeNumber directoryId tb;
	
String64 :: String;
String64	= createArray 64 '@';

//--
//import StdDebug, dodebug

GetLongPathName :: !String -> String;
GetLongPathName s
//	#! (_,_,tb) = GetFName s 42
//	= trace_n s s
	= s

GetShortPathName :: !String -> (!Bool,!String);
GetShortPathName s = (True,s)//quoted_string s)

//--
GetFName :: !.{#Char} !*Toolbox -> (!Int,!Int,!*Toolbox);
GetFName ioNamePtr t = code (ioNamePtr=R80O0D0SD1,t=U)(ioResult=D0,ioDate_and_Time=I80A0,t2=Z){
	instruction 0x39400000	|	li	r10,0
	instruction 0x9141000C	|	stw	r10,12(sp)
	instruction 0x90810012	|	stw	r4,18(sp)
	instruction 0xB1410016	|	sth	r10,22(sp)
	instruction 0xB141001A	|	sth	r10,26(sp)
	instruction 0x91410030	|	stw	r10,48(sp)
	instruction 0x3940FFFF	|	li	r10,-1
	instruction 0xB141001C	|	sth	r10,28(sp)
	call	.PBHGetFInfoSync
	instruction 0x8321004C	|	lwz	r25,76(sp)
	};

import StdFileSelect, StdPSt, StdPStClass

selectDirectory` :: !(PSt .l) -> (!Maybe String,!PSt .l)
selectDirectory` ps
	# (ms,ps)	= selectDirectory ps
	# ms		= mapMaybe removeDirsep ms
	= (ms,ps)
where
	removeDirsep s
		# final				= dec (size s)
		| s.[final] == ':'	= s%(0,dec final)
		= s

ShellDefault :: !{#Char} !(PSt .l) -> (!Int,!(PSt .l))
ShellDefault _ ps = abort "no ShellDefault on a Mac silly:-)"
