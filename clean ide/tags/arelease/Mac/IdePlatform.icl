implementation module IdePlatform

import StdPSt
import StdArray, StdEnum, StdList
import StdFunc
import PmCleanSystem
import errwin
from IdeState import General

PlatformProcessAttributes :: [ProcessAttribute *(PSt General)]
PlatformProcessAttributes = []

RunProgram :: !.String !*(PSt General) -> *PSt General
RunProgram path ps
	#	(project,ps)	= getProject ps
		ao				= PR_GetApplicationOptions project
		(ps,_)			= Execute updateErrorWindow path ao ps
	= ps

