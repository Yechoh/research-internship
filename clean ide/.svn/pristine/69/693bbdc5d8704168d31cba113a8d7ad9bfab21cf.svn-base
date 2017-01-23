definition module IdePlatform

import StdPSt, StdString
import StdIOCommon
from IdeState import :: General

PlatformInteractiveInit		:: !*(PSt General) -> *PSt General
PlatformProcessAttributes	:: [ProcessAttribute *(PSt General)]
RunProgram					:: !.String !*(PSt General) -> *PSt General

SetWindowIcon :: !Id !Int !(PSt .l) -> PSt .l
SetProcessIcon :: !Int !(PSt .l) -> PSt .l
GetDialogBackgroundColour :: !(PSt .l) -> (!Colour, !PSt .l)

ProjectIcon	:== 32513
ImpmodIcon	:== 32516
DefmodIcon	:== 32515
CleanIcon	:== 32512
AbcmodIcon	:== 32514

toolIconFun :: !String !(Maybe String) !(IdFun .st) ![(ToolbarItem .st)] !*env -> (![(ToolbarItem .st)],!*env)  | FileSystem env

AboutBitmap	:== ""
findBM		:== "findBM.bmp"
newfBM		:== "newfBM.bmp"
openBM		:== "openBM.bmp"
prntBM		:== "prntBM.bmp"
saveBM		:== "saveBM.bmp"
srchBM		:== "srchBM.bmp"
updtBM		:== "updtBM.bmp"
urunBM		:== "urunBM.bmp"
