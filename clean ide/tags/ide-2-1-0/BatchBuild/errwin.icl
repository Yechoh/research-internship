implementation module errwin

import StdString, StdList, StdFunc
import StdPSt, IdeState

updateErrorWindow :: !([String]) !*(PSt *General) -> *PSt *General;
updateErrorWindow s ps = seq (map writeLog s) ps

ew_safe_close :: !*(PSt *General) -> *PSt *General
ew_safe_close ps = ps
