definition module Platform

import StdPSt, StdString

PlatformDependant win mac :== win

initPlatformCommandLine			:: !*(PSt .l) -> (![String],!*PSt .l)
installPlatformEventHandlers	:: !*(PSt .l) -> *(PSt .l)
openPlatformWindowMenu			:: !*(PSt .l) -> *(PSt .l)

ToolsDir	:: String
TempDir		:: String
EnvsDir		:: String
PrefsDir	:: String

batchOptions :: !*World -> (!Bool,Bool,String,*File,!*World)
wAbort :: !String !*World -> *World
