implementation module messwin

import StdString, StdPSt, StdBool, StdList, StdFunc
from IdeState import :: General, writeLog

:: Info
	= Level1 String
	| Level2 String
	| Level3 [String]

showInfo :: !.Info !*(PSt General) -> !*PSt General
showInfo info  ps
	= case info of
		(Level1 s)	-> writeLog s ps
		(Level2 s)	-> writeLog s ps
		(Level3 s)	-> seq (map writeLog s) ps

closeInfo :: !*(PSt General) -> !*PSt General
closeInfo ps
	= ps
