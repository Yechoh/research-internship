definition module Platform

import StdPSt, StdString
import StdIOCommon

PlatformDependant win mac :== mac

initPlatformCommandLineArguments:: !*(PSt .l) -> (![String],!*PSt .l)
installPlatformEventHandlers	:: !*(PSt .l) -> *(PSt .l)
openPlatformWindowMenu			:: !*(PSt .l) -> *(PSt .l)

getWindowModified :: !Id !(IOSt .l) -> (!Maybe Bool,!IOSt .l)
setWindowModified :: !Id !String !Bool !(IOSt .l) -> IOSt .l

TempDir	:: String
EnvsDir		:: String
PrefsDir	:: String
BitmapDir	:: String

batchOptions	:: !*World -> (!Bool,Bool,String,*File,!*World)
wAbort			:: !String !*World -> *World
pAbort			:: !(PSt .a) -> PSt .a

onOSX	:: Bool
