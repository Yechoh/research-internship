implementation module callCpm

import iTasks
import shares
import qualified System.Process as SP
import extraTaskCombinators
import directoryBrowsing
import qualified Data.Map as DMh

cpmCreateProject :: String -> Task ()
cpmCreateProject projname =
	get settings >>- \settings ->
	(appWorld (\w. snd ('SP'.callProcess settings.dirCpm ["project",projname,"create"] Nothing w))) 
	
cpmSetErrorstate :: String String -> Task ()
cpmSetErrorstate path iclname =
	(contentOf iclname) >>- \c ->
	saveFile (path </> iclname) c >>|- 
	get settings >>- \settings ->
	appWorld (\w. snd ('SP'.callProcess settings.dirCmd ["start","/c",settings.dirCpm,toproj iclname,"--envs="+++settings.dirIDEEnvs,"1>templog.txt","2>errlog.txt"] Nothing w))
	>>|- readFromFile "templog.txt"
	>>- \(Just errors). 
	set (errors) errorstate
	>>|- return ()
