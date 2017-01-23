definition module Platform

import StdPSt, StdString
import StdIOCommon
from IdeState import General

PlatformDependant win mac :== mac

initPlatformCommandLine			:: !*(PSt .l) -> (![String],!*PSt .l)
installPlatformEventHandlers	:: !*(PSt .l) -> *(PSt .l)
openPlatformWindowMenu			:: !*(PSt .l) -> *(PSt .l)
