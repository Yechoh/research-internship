implementation module callCpm

import iTasks
import shares
import qualified System.Process as SP
import extraTaskCombinators
import directoryBrowsing

cpmCreateProject :: String -> Task ()
cpmCreateProject projname =
	get settings >>- \settings ->
	(appWorld (\w. snd ('SP'.callProcess settings.dirCpm ["project",projname,"create"] Nothing w))) 
	
cpmSetErrorstate :: String -> Task ()
cpmSetErrorstate projname =
	get settings >>- \settings ->
	appWorld (\w. snd ('SP'.callProcess settings.dirCmd ["start","/c",settings.dirCpm,projname,"--envs="+++settings.dirIDEEnvs,"1>templog.txt","2>errlog.txt"] Nothing w))
	>>|- readFromFile "templog.txt"
	>>- \(Just errors). 
	set (errors) errorstate
	>>|- return ()