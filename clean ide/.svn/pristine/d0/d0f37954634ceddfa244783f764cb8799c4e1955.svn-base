implementation module Platform

import StdInt, StdMisc, StdFile
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

batchOptions	:: !*World -> (!Bool,Bool,String,*File,!*World)
batchOptions world
	# interact		= True
	  force_update	= undef
	  prj_name		= undef
	  logfile		= undef
	= (interact,force_update,prj_name,logfile,world)

wAbort			:: !String !*World -> *World
wAbort message world
	# stderr	= fwrites message stderr
	# (_,world)	= fclose stderr world
	= world

install_apple_event_handlers :: Int
install_apple_event_handlers
	= code ()(r=D0) {
		call	.install_apple_event_handlers
	}

TooltempDir	:: String
TooltempDir = applicationpath "Temp"

EnvsDir		:: String
EnvsDir = applicationpath "Config"

PrefsDir	:: String
PrefsDir = applicationpath "Config"

BitmapDir	:: String
BitmapDir = applicationpath "Bitmaps"
