implementation module messwin

import StdString, StdPSt, StdBool, StdList, StdFunc
from IdeState import :: General, writeLog

:: InfoMessage
	= Level1 String
	| Level2 String
	| Level3 [String]

showInfo :: !.InfoMessage !*(PSt General) -> !*PSt General
showInfo info  ps
	= case info of
		(Level1 s)	-> writeLog s ps
		(Level2 s)	-> writeLog s ps
		(Level3 s)	-> seq (map writeLog s) ps

closeInfo :: !*(PSt General) -> !*PSt General
closeInfo ps
	= ps
