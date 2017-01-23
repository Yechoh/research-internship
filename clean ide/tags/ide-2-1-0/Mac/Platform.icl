implementation module Platform

import StdInt, StdMisc, StdFile
import StdTuple,StdOverloaded,StdArray
import StdPSt
import StdIOCommon
import StdSystem

PlatformDependant win mac
	:== mac

initPlatformCommandLine :: !*(PSt .l) -> (![String],!*PSt .l)
initPlatformCommandLine ps
	= ([],ps)

installPlatformEventHandlers :: !*(PSt .l) -> *(PSt .l)
installPlatformEventHandlers ps
	| install_apple_event_handlers == 0
//		= trace_n "apple events installed" ps
		= ps
//	= trace_n "installing apple events failed :-(" ps
	= ps

openPlatformWindowMenu :: !*(PSt .l) -> *(PSt .l)
openPlatformWindowMenu ps
	= ps
//	= openWindowMenu ps

// FIXME: should be IdePlatform
batchOptions	:: !*World -> (!Bool,Bool,String,*File,!*World)
batchOptions world
	# interact		= True
	  force_update	= undef
	  prj_name		= undef
	  logfile		= undef
	= (interact,force_update,prj_name,logfile,world)

// FIXME: should be IdePlatform
wAbort			:: !String !*World -> *World
wAbort message world
	# stderr	= fwrites message stderr
	# (_,world)	= fclose stderr world
	= world

// FIXME: should be IdePlatform
pAbort			:: !(PSt .a) -> PSt .a
pAbort ps = ps

install_apple_event_handlers :: Int
install_apple_event_handlers
	= code ()(r=D0) {
		call	.install_apple_event_handlers
	}

TempDir	:: String
TempDir = applicationpath "Temp"

EnvsDir		:: String
EnvsDir = applicationpath "Config"

PrefsDir	:: String
PrefsDir = applicationpath "Config"

BitmapDir	:: String
BitmapDir = applicationpath "Bitmaps"

//////////////

//import dodebug
trace_n` _ f :== f

import windowaccess, iostate, StdBool,menuwindowmenu
import code from library "winmod_library"

getWindowModified :: !Id !(IOSt .l) -> (!Maybe Bool,!IOSt .l)
getWindowModified id ioState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= windowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,ioStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (mod,wsH,ioState)		= getWindowModified wsH ioState
		= (Just mod,ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
	getWindowModified wsH=:{wshIds={wPtr}} ioState
		# (mod,ioState)			= accIOToolbox (IsWindowModified wPtr) ioState
		= trace_n` ("getWindowModified",wPtr,mod) (mod<>0,wsH,ioState)
	
	IsWindowModified :: !OSWindowPtr !*OSToolbox -> (!Int,!*OSToolbox)
	IsWindowModified wPtr ioState = code {
		ccall IsWindowModified "PI:I:I"
		}

setWindowModified :: !Id !String !Bool !(IOSt .l) -> IOSt .l
setWindowModified id windowName mod ioState
	# windowTitle				= if mod ("¥"+++windowName) windowName
	# ioState					= changeWindowInWindowMenu id windowTitle ioState

	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= windowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= ioStSetDevice (WindowSystemState windows) ioState
	| otherwise
		# (wsH,ioState)			= setWindowModified wsH mod ioState
		= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setWindowModified wsH=:{wshIds={wPtr}} mod ioState
		# (err,ioState)			= accIOToolbox (SetWindowModified wPtr (if mod (1 << 24) 0)) ioState
		= trace_n` ("setWindowModified",wPtr,mod,err) (wsH,ioState)
	
	SetWindowModified :: !OSWindowPtr !Int !*OSToolbox -> (!OSStatus,!*OSToolbox)
	SetWindowModified wPtr mod ioState = code {
		ccall SetWindowModified "PII:I:I"
		}

:: OSStatus	:== Int

onOSX :: Bool
onOSX =: fst (runningCarbonOSX OSNewToolbox)

runningCarbonOSX tb
	# (err,res,tb)	= Gestalt "sysv" tb
	| err <> 0 = abort "Gestalt failed.\n"
	= (res >= 0x01000, tb)

Gestalt :: !String !*Int -> (!Int,!Int,!*Int)
Gestalt sSel tb
	| size sSel <> 4 = abort "Gestalt not called with four-char selector.\n"
	# iSel	= ((toInt sSel.[0]) << 24) bitor ((toInt sSel.[1]) << 16) bitor ((toInt sSel.[2]) << 8) bitor ((toInt sSel.[3]) << 0)
	= Gestalt iSel tb
where
	Gestalt :: !Int !*Int -> (!Int,!Int,!*Int)
	Gestalt _ _ = code {
		ccall Gestalt "PI:II:I"
		}
