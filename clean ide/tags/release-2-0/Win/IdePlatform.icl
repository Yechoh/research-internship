implementation module IdePlatform

import StdEnv
import StdPSt, StdPStClass
import PmCleanSystem
import errwin, conswin
import UtilIO
import EdKeyboard, EdMouse

from iostate	import appIOToolbox,accIOToolbox
from clCCall_12	import winLaunchApp,winMakeCString,:: CSTR,:: OSToolbox
import	clCrossCall_12, windowaccess, iostate
import pictCCall_12, cast

import code from "Redirect.obj"
import code from "cCrossCallMaarten.obj"

import code from library "conkernel_library"
import code from library "bmpgdi_library"

PlatformProcessAttributes :: [ProcessAttribute *(PSt General)]
PlatformProcessAttributes = 
	[ ProcessConsoleOpen id
	, ProcessConsoleQuit consoleKill
	, ProcessConsoleOut  consoleMessageO
	, ProcessConsoleErr  consoleMessageE
	]

//-- Experimental Console Handling/Redirection...

RunProgram :: !.String !*(PSt General) -> *PSt General
RunProgram path ps
	# (ret,ps) = accPIO (accIOToolbox (AddMainWindowHook True)) ps
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
		#	(didit,_) = startChildProcess (quoted_string path +++. " -con") False/*True*/ 99
		| didit
			= (ps,True)
		= (winfun ["Error: Could not launch the console application."] ps,False)

//--

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

AddMainWindowHook :: !Bool !*OSToolbox -> (!Bool,!*OSToolbox)
AddMainWindowHook _ tb = code {
		ccall AddMainWindowHook "I:I:I"
	}
	
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

winInitialiseTooltips :: !*OSToolbox -> *OSToolbox
winInitialiseTooltips _
	= code
	{
		.inline InstallCrossCallMaarten
			ccall InstallCrossCallMaarten "I-I"
		.end
	}

osIgnoreCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
osIgnoreCallback _ tb 
	= (return0Cci,tb)

//--

CcRqGETBITMAPRESOURCE	:== 1477

osGetBitmapResource :: !Int !*OSToolbox -> (!(!Int,!String,!Int,!Int),!*OSToolbox)
osGetBitmapResource bitmap_id tb
	# (hbitmap,w,h,tb) = winGetBitmapResource bitmap_id tb
	# data = ""
	= ((hbitmap,data,w,h),tb)
	
winGetBitmapResource :: !Int !*OSToolbox -> (!Int,!Int,!Int,!*OSToolbox)
winGetBitmapResource _ _ = code {
		ccall WinGetBitmapResource "II-IIII"
	}

GetBitmapResource :: !Int !.env -> (!Maybe Bitmap,!.env)
GetBitmapResource bitmap_id ps
	# ((hbmp,data,w,h),_)	= osGetBitmapResource bitmap_id OSNewToolbox
	| hbmp == 0			= (Nothing, ps)
	# osbmp		= {originalSize=(w,h),reSize=(w,h),bitmapContents=data,bitmapHandle=hbmp}
	= (Just (toBitmap osbmp), ps)

//--

CcRqSETWINDOWICON		:== 1474

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
	
CleanIcon	:== 32512
ProjectIcon	:== 32513
AbcmodIcon	:== 32514
DefmodIcon	:== 32515
ImpmodIcon	:== 32516

AboutBitmap	:== 32512
findBM		:== 32513
newfBM		:== 32514
openBM		:== 32515
prntBM		:== 32516
saveBM		:== 32517
srchBM		:== 32518
updtBM		:== 32519
urunBM		:== 32520

//===

COLOR_SCROLLBAR         :== 0
COLOR_BACKGROUND        :== 1
COLOR_ACTIVECAPTION     :== 2
COLOR_INACTIVECAPTION   :== 3
COLOR_MENU              :== 4
COLOR_WINDOW            :== 5
COLOR_WINDOWFRAME       :== 6
COLOR_MENUTEXT          :== 7
COLOR_WINDOWTEXT        :== 8
COLOR_CAPTIONTEXT       :== 9
COLOR_ACTIVEBORDER      :== 10
COLOR_INACTIVEBORDER    :== 11
COLOR_APPWORKSPACE      :== 12
COLOR_HIGHLIGHT         :== 13
COLOR_HIGHLIGHTTEXT     :== 14
COLOR_BTNFACE           :== 15
COLOR_BTNSHADOW         :== 16
COLOR_GRAYTEXT          :== 17
COLOR_BTNTEXT           :== 18
COLOR_INACTIVECAPTIONTEXT :== 19
COLOR_BTNHIGHLIGHT      :== 20

COLOR_3DDKSHADOW        :== 21
COLOR_3DLIGHT           :== 22
COLOR_INFOTEXT          :== 23
COLOR_INFOBK            :== 24

COLOR_HOTLIGHT                  :== 26
COLOR_GRADIENTACTIVECAPTION     :== 27
COLOR_GRADIENTINACTIVECAPTION   :== 28

COLOR_DESKTOP           :== COLOR_BACKGROUND
COLOR_3DFACE            :== COLOR_BTNFACE
COLOR_3DSHADOW          :== COLOR_BTNSHADOW
COLOR_3DHIGHLIGHT       :== COLOR_BTNHIGHLIGHT
COLOR_3DHILIGHT         :== COLOR_BTNHIGHLIGHT
COLOR_BTNHILIGHT        :== COLOR_BTNHIGHLIGHT

GetSysColor :: !Int -> Int
GetSysColor nIndex = code {
 ccall GetSysColor@4 "PI:I"
 }

GetDialogBackgroundColour :: !(PSt .l) -> (!Colour, !PSt .l)
GetDialogBackgroundColour ps
	= (RGB {r = rcol, g = gcol, b = bcol}, ps)
where
 col  = GetSysColor COLOR_BTNFACE
 rcol = (col bitand 0x000000FF)
 gcol = (col bitand 0x0000FF00) >> 8
 bcol = (col bitand 0x00FF0000) >> 16

