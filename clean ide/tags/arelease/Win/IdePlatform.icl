implementation module IdePlatform

import StdPSt
import StdArray, StdEnum, StdList, StdTuple
import StdFunc

PlatformProcessAttributes :: [ProcessAttribute *(PSt General)]
PlatformProcessAttributes = 
//	[]/*
	[ ProcessConsoleOpen id
	, ProcessConsoleQuit consoleKill
	, ProcessConsoleOut  consoleMessageO
	, ProcessConsoleErr  consoleMessageE
	]
//*/

//-- Experimental Console Handling/Redirection...
from clCCall_12 import winLaunchApp,OSToolbox
import StdBool
import PmCleanSystem
import errwin
import UtilIO, StdPStClass

RunProgram :: !.String !*(PSt General) -> *PSt General
RunProgram path ps
	#	(project,ps)	= getProject ps
		(redc,ps)		= getCurrentRedc ps
		ao				= PR_GetApplicationOptions project
		(ps,_)			= Execute` redc updateErrorWindow path ao ps
	= ps
where
	// can remove from PmCleanSystem?!
	Execute ::	!(WindowFun *env) !Pathname !ApplicationOptions *env -> (*env, !Bool)
	Execute winfun path {o} ps
		#	(didit,_) = winLaunchApp (quoted_string path) (o<>NoConsole) 99
		| didit
			= (ps,True)
			= (winfun ["Error: Could not launch the application."] ps,False)

	Execute` ::	!Bool !(WindowFun *(PSt General)) !Pathname !ApplicationOptions *(PSt General) -> (*PSt General, !Bool)
	Execute` redc winfun path {o} ps
		# (exists,ps)	= accFiles (FExists path) ps
		| not exists
			= (winfun ["Error: No application to run, you must bring the project up to date."] ps, False)
		| not redc || o == NoConsole
			#	(didit,_) = winLaunchApp (quoted_string path) (o<>NoConsole) 99
			| didit
				= (ps,True)
			= (winfun ["Error: Could not launch the application."] ps,False)
// need to investigate here...
		#	(didit,_) = winLaunchApp (quoted_string path) (o<>NoConsole) 99
//		#	(didit,_) = startChildProcess (quoted_string path +++. " -con") False/*True*/ 99
		| didit
//			#	ps	= consoleMessageE ("<"+++.path+++." launched>\n") ps
			= (ps,True)
		= (winfun ["Error: Could not launch the console application."] ps,False)
/*
SetWindowIcon :: !Id !Int !(PSt .l) -> PSt .l
SetWindowIcon wId icon pState = pState
*/
//*
import conswin

import EdKeyboard, EdMouse
//--
from iostate import appIOToolbox,accIOToolbox

consWinKeyboard :: .WindowAttribute *(EditState,*PSt *General);
consWinKeyboard = WindowKeyboard	(\ks -> getKeyboardStateKeyState ks == KeyDown False) Able consKeyboard

consWinMouse :: .WindowAttribute *(EditState,*PSt *General);
consWinMouse = WindowMouse noMouseMoved Able editWindowMouse

consoleMessageI :: !{#Char} !(PSt General) -> PSt General
consoleMessageI msg ps = updateConsoleWindowI msg [consWinKeyboard,consWinMouse] ps

consoleMessageO :: !{#Char} !(PSt General) -> PSt General
consoleMessageO msg ps = updateConsoleWindowO msg [consWinKeyboard,consWinMouse] ps

consoleMessageE :: !{#Char} !(PSt General) -> PSt General
consoleMessageE msg ps = updateConsoleWindowE msg [consWinKeyboard,consWinMouse] ps

consoleKill :: !(PSt .a) -> (PSt .a)
consoleKill ps
	# ps = appPIO (appIOToolbox (killChildProcess)) ps
	= ps

consKeyboard :: .KeyboardState *(EditState,*PSt *General) -> *(EditState,*PSt *General);
consKeyboard ks (es,ps)
	| ks == SpecialKey f1Key (KeyDown False) ControlOnly
		# ps = appPIO (appIOToolbox (killChildProcess)) ps
		= (es,ps)
	| ks == SpecialKey f2Key (KeyDown False) ControlOnly
		# (_,ps) = accPIO (accIOToolbox (writeChildProcess "X")) ps
		= (es,ps)
	= case ks of
		(CharKey char _)
			# (_,ps) = accPIO (accIOToolbox (writeChildProcess {char})) ps
			# ps = consoleMessageI {char} ps
			-> (es,ps)
		(SpecialKey key _ _)
			| key == enterKey
				# (_,ps) = accPIO (accIOToolbox (writeChildProcess "\n")) ps
				# ps = consoleMessageI "\n" ps
				-> (es,ps)
			# (ed,ps)				= getEditorState ps
			# keyMapping			= getKeyMapping ed
			-> noeditWindowKeyboard keyMapping ks (es,ps)

//-- Console bindings...

import code from library "conkernel_library"
import code from "Redirect.obj"
from clCCall_12 import winMakeCString,CSTR,OSToolbox

startChildProcess :: !{#Char} !Bool !*OSToolbox -> (!Bool,!*OSToolbox)
startChildProcess cmdl swin tb
	# (cstr,tb) = winMakeCString cmdl tb
	= (startChildProcess` cstr swin,tb)

startChildProcess` :: !CSTR !Bool -> !Bool
startChildProcess` _ _
	= code
	{
		.inline StartChildProcess
			ccall StartChildProcess "II-I"
		.end
	}

killChildProcess :: !*OSToolbox -> *OSToolbox
killChildProcess _
	= code
	{
		.inline TerminateChildProcess
			ccall TerminateChildProcess "I-I"
		.end
	}

writeChildProcess :: !{#Char} !*OSToolbox -> (!Int,!*OSToolbox)
writeChildProcess str tb
	# (cstr,tb) = winMakeCString str tb
	= (writeChildProcess` cstr,tb)

writeChildProcess` :: !CSTR -> !Int
writeChildProcess` cstr
	= code
	{
		.inline WriteChildStdIn
			ccall WriteChildStdIn "I-I"
		.end
	}

//==


import	clCrossCall_12, windowaccess, iostate
import code from "cCrossCallMaarten.obj"

winInitialiseTooltips :: !*OSToolbox -> *OSToolbox
winInitialiseTooltips _
	= code
	{
		.inline InstallCrossCallMaarten
			ccall InstallCrossCallMaarten "I-I"
		.end
	}

CcRqSETWINDOWICON	:== 1474

osIgnoreCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
osIgnoreCallback _ tb 
	= (return0Cci,tb)

osSetWindowIcon :: !OSWindowPtr !Int !*OSToolbox -> *OSToolbox
osSetWindowIcon wPtr icon_id tb
	# tb = winInitialiseTooltips tb
	= snd (issueCleanRequest2 osIgnoreCallback (Rq2Cci CcRqSETWINDOWICON wPtr icon_id) tb)

SetWindowIcon :: !Id !Int !(PSt .l) -> PSt .l
SetWindowIcon wId icon pState=:{io}
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice io
	| not found
		= {pState & io=ioState}
	# windows					= windowSystemStateGetWindowHandles wDevice
	# (found,wsH,windows)		= getWindowHandlesWindow (toWID wId) windows
	| not found
		# windows				= setWindowHandlesWindow wsH windows
		# ioState				= ioStSetDevice (WindowSystemState windows) ioState
		= {pState & io=ioState}
	| otherwise
		#! wPtr					= wsH.wshIds.wPtr
		# ioState				= appIOToolbox (tbfun wPtr) ioState
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= ioStSetDevice (WindowSystemState windows) ioState
		= {pState & io=ioState}
where
	tbfun wPtr tb
		# tb = osSetWindowIcon wPtr icon tb
		= tb

SetProcessIcon :: !Int !(PSt .l) -> PSt .l
SetProcessIcon icon pState=:{io=ioState}
	# (osdi,ioState)	= ioStGetOSDInfo ioState
	# mosi				= getOSDInfoOSInfo osdi
	| isNothing mosi
		# ioState		= ioStSetOSDInfo osdi ioState
		= {pState & io = ioState}
	# {osFrame}			= fromJust mosi
	# ioState			= appIOToolbox (osSetWindowIcon osFrame icon) ioState
	# ioState			= ioStSetOSDInfo osdi ioState
	= {pState & io = ioState}
	
//*/
ProjectIcon	:== 32513
ImpmodIcon	:== 32516
DefmodIcon	:== 32515
CleanIcon	:== 32512
AbcmodIcon	:== 32514
