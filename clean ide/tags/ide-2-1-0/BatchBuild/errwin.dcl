definition module errwin

import StdString

import StdPSt, IdeState

updateErrorWindow :: !([String]) !*(PSt *General) -> *PSt *General;
ew_safe_close			:: !*(PSt *General) -> *PSt *General
