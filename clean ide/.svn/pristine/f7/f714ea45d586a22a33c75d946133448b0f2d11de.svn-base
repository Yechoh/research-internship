implementation module IdePlatform

import StdPSt
import StdArray, StdEnum, StdList
import StdFunc
import PmCleanSystem
import errwin
from IdeState import :: General
import Platform

toolIconFun :: !String !(Maybe String) !(IdFun .st) ![(ToolbarItem .st)] !*env -> (![(ToolbarItem .st)],!*env) | FileSystem env
toolIconFun bitmapname tooltip toolfun itemlist world
/* Disable for now as toolbars are not implemented yet!
	# bitmappath	= MakeFullPathname BitmapDir bitmapname
	# (bmp,world)	= openBitmap bitmappath world
	# itemlist		= case bmp of
						Nothing		-> itemlist
						Just bmp	-> [ToolbarItem bmp tooltip toolfun:itemlist]
*/
	= (itemlist,world)

PlatformInteractiveInit		:: !*(PSt General) -> *PSt General
PlatformInteractiveInit ps
	= ps

GetDialogBackgroundColour :: !(PSt .l) -> (!Colour,!PSt .l)
GetDialogBackgroundColour ps
	= (White/*LightGrey*/, ps)	// Mac Appearance dependant!

PlatformProcessAttributes :: [ProcessAttribute *(PSt General)]
PlatformProcessAttributes = []

RunProgram :: !.String !*(PSt General) -> *PSt General
RunProgram path ps
	#	(project,ps)	= getProject ps
		ao				= PR_GetApplicationOptions project
		(ps,_)			= Execute updateErrorWindow path ao ps
	= ps

//-- Win only for now ?!

SetWindowIcon :: !Id !Int !(PSt .l) -> PSt .l
SetWindowIcon wId iId ps = ps

SetProcessIcon :: !Int !(PSt .l) -> PSt .l
SetProcessIcon iId ps = ps

ProjectIcon	:== 32513
ImpmodIcon	:== 32516
DefmodIcon	:== 32515
CleanIcon	:== 32512
AbcmodIcon	:== 32514

AboutBitmap	:== ""
findBM		:== "findBM.bmp"
newfBM		:== "newfBM.bmp"
openBM		:== "openBM.bmp"
prntBM		:== "prntBM.bmp"
saveBM		:== "saveBM.bmp"
srchBM		:== "srchBM.bmp"
updtBM		:== "updtBM.bmp"
urunBM		:== "urunBM.bmp"
