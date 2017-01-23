definition module messwin

import StdString, StdPSt
from IdeState import General

:: Info
	= Level1 String
	| Level2 String
	| Level3 [String]

showInfo	:: !.Info !*(PSt General) -> !*PSt General
closeInfo	:: !*(PSt General) -> !*PSt General
