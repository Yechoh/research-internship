definition module Directory

// version 1.1

from StdFile		import class FileSystem
from StdOverloaded	import class toInt, class ==

// shorthands: PI="platform independent", PD="platform dependent"

getDirectoryContents :: !Path !*env -> (!(!DirError, [DirEntry]), !*env)  | FileSystem env

	/*	error codes: NoDirError, DoesntExist, BadName, NoPermission, OtherErro
		The returned list is only valid, if the error code is NoDirError */

isHidden :: !DirEntry -> Bool

	/*	unix: whether the filename's first character is '.'
		windows/mac: the win(mac)IsHidden flag from the pd_fileInfo field */
	
getFileInfo :: !Path !*env -> (!(!DirError, FileInfo), !*env)  | FileSystem env

	/*	get information about a given file/directory.
		error codes: NoDirError, DoesntExist, BadName, NoPermission, OtherDirError
		The returned FileInfo is only valid, if the error code is NoDirError */

createDirectory :: !Path !*env -> (!DirError, !*env)  | FileSystem env

	/*	error codes: NoDirError, DoesntExist, BadName, NotEnoughSpace, AlreadyExists, NoPermission,
					 OtherDirError */

fmove :: !MoveMode !Path !Path !*env -> (!DirError, !*env)  | FileSystem env

	/*	(fmove mode src dest) moves a file or directory src to another location dst. dst also contains
		the possibly new name of the moved object. Iff (dst already exists and dst is not a directory
		and	mode is OverwriteFile) then dst will be replaced by src.
		error codes: NoDirError, DoesntExist, BadName, NotEnoughSpace, AlreadyExists, NoPermission,
					 MoveIntoOffspring, MoveAcrossDisks, OtherDirError */

fremove :: !Path !*env -> (!DirError, !*env)  | FileSystem env

	/*	removes files and empty directories.
		error codes: NoDirError, DoesntExist, BadName, NoPermission, NotYetRemovable, OtherDirError */

getCurrentDirectory :: !*env -> (!Path, !*env)           | FileSystem env

setCurrentDirectory :: !Path !*env -> (!DirError, !*env) | FileSystem env

	/*	error codes: NoDirError, DoesntExist, BadName, NoPermission, OtherDirError */

getDiskNames :: !*env -> ([DiskName], !*env)             | FileSystem env

pd_StringToPath :: !String !*env -> (!(!Bool, Path), !*env) | FileSystem env

	/*	converts a platform dependent string representation of a path into a Path which
		is only valid, if the returned Bool is True */ 
	
pathToPD_String :: !Path !*env -> (!String,  !*env)      | FileSystem env

	// converts a Path into a platform dependent string representation of a path

encodeUnixModeBits	::	!UnixModeBitsField -> UnixAccessRights
instance == DirError

::	Path
	=	RelativePath [PathStep]
	|	AbsolutePath DiskName [PathStep]

::	PathStep	= PathUp | PathDown String
::	DiskName	:== String
	/*	on UNI//Name is ignored, on DOS/Windows/MacOS the ":" is omitted,
		Windows network paths are specified as an AbsolutePath whose DiskName begins with two
		backslashes (\\) */
	
::	DirError = NoDirError | DoesntExist | BadName | NotEnoughSpace | AlreadyExists | NoPermission
				| MoveIntoOffspring | MoveAcrossDisks | NotYetRemovable | OtherDirError

	/* NotYetRemovable: a file can't be removed because it has not been closed yet or
						a directory can't be removed because it is not empty */

::	DirEntry
	=	{	fileName		::	!String
		,	fileInfo		::	!FileInfo
		}

::	FileInfo
	=	{	pi_fileInfo		::	!PI_FileInfo
		,	pd_fileInfo		::	!PD_FileInfo
		}

::	PI_FileInfo								//	platform independent information
	=	{	fileSize		::	!Integer64
		,	lastModified	::	!DateTime
		,	isDirectory		::	!Bool
		,	isReadOnly		::	!Bool
		}

::	PD_FileInfo	:== PlatformDependent UnixFileInfo WindowsFileInfo MacFileInfo
											//	platform dependent information

::	PlatformDependent unix windows mac
	=	Unix	unix
	|	Windows	windows
	|	Mac		mac

::	UnixFileInfo
	= 	{	unixModeBitsField	::	!UnixModeBitsField
		,	unixOwnerUserId		::	Int
		,	unixOwnerGroupId	::	Int
		,	unixLastAccessTime	::	!DateTime
		}
		
::	UnixModeBitsField	:==	Int

::	UnixAccessRights
	=	{	ownerRight			::	UnixRight
		,	groupRight			::	UnixRight
		,	othersRight			::	UnixRight
		}

::	UnixRight
	=	{	mayRead				::	!Bool
		,	mayWrite			::	!Bool
		,	mayExecute			::	!Bool
		}
		
::	WindowsFileInfo
	=	{	winCreationTime		::	!DateTime
		,	winLastAccessTime	::	!DateTime
		,	winDOSName			::	!String
		,	winIsHidden			::	!Bool
		,	winIsArchiveFile	::	!Bool
		,	winIsSystemFile		::	!Bool
		}

::	MacFileInfo
	=	{	macCreationTime		::	!DateTime
		,	macBackupTime		::	!DateTime
		,	macIsHidden			::	!Bool
		,	macFDFlags			::	!Int	//	finder information from FInfo/DInfo record
		,	macFurtherInfo		::	!ForFile MacFurtherInfo
		}

::	MacFurtherInfo	:==	(!MacCreator, !MacFileType)

::	MacCreator	:==	String		//	always four characters
::	MacFileType	:==	String		//	always four characters

::	MoveMode	= OverwriteFile | DontOverwrite
::	DateTime	:==	(!Date`, !Time`)

::	ForFile x
	=	File` x
	|	Directory

::	Integer64			// currently 64 Bit arithmetic, will be replaced
						// with arbitrary precision arithmethic in the future

instance toInt Integer64	// will truncate values>2^31-1 -> loss of information

// A similar type is also defined in module StdTime (Object I/O library)

::	Time`
	=	{	hours`	:: !Int		// hours		(0-23)
		,	minutes`:: !Int		// minutes		(0-59)
		,	seconds`:: !Int		// seconds		(0-59)
		}
::	Date`
	=	{	year`	:: !Int		// year
		,	month`	:: !Int		// month		(1-12)
		,	day`	:: !Int		// day			(1-31)
		,	dayNr`	:: !Int		// day of week	(1-7, Sunday=1, Saturday=7)
		}
