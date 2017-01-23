definition module messwin

// Display PmDriver messages...

import StdPSt
from IdeState import :: General

:: Info					// message levels
	= Level1 String
	| Level2 String
	| Level3 [String]

showInfo	:: !.Info !*(PSt General) -> !*PSt General
closeInfo	:: !*(PSt General) -> !*PSt General
