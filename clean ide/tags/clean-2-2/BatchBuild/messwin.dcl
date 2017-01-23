definition module messwin

import StdString, StdPSt
from IdeState import :: General

:: InfoMessage
	= Level1 String
	| Level2 String
	| Level3 [String]

showInfo	:: !.InfoMessage !*(PSt General) -> !*PSt General
closeInfo	:: !*(PSt General) -> !*PSt General
