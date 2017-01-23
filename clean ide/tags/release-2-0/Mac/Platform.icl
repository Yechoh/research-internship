implementation module Platform

import StdInt
import StdPSt
//import menuwindowmenu
import PmCleanSystem
import StdIOCommon, IdeState
import errwin

PlatformDependant w m :== m

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

install_apple_event_handlers :: Int
install_apple_event_handlers
	= code ()(r=D0) {
		call	.install_apple_event_handlers
	}

