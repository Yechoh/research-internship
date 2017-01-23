definition module Platform

import StdPSt, StdString
import StdIOCommon

PlatformDependant win mac :== mac

initPlatformCommandLine			:: !*(PSt .l) -> (![String],!*PSt .l)
installPlatformEventHandlers	:: !*(PSt .l) -> *(PSt .l)
openPlatformWindowMenu			:: !*(PSt .l) -> *(PSt .l)

TooltempDir	:: String
EnvsDir		:: String
PrefsDir	:: String
BitmapDir	:: String

batchOptions	:: !*World -> (!Bool,Bool,String,*File,!*World)
wAbort			:: !String !*World -> *World
