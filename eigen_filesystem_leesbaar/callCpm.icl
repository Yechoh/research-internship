implementation module callCpm

import iTasks
import shares
import qualified System.Process as SP
import extraTaskCombinators
import directoryBrowsing
import qualified Data.Map as DM

cpmCreateProject :: String -> Task ()
cpmCreateProject projname =
	get settings >>- \settings ->
	(appWorld (\w. snd ('SP'.callProcess settings.cpmDir ["project",projname,"create"] Nothing w))) 
	
cpmSetErrorstateWithErrlog :: Task ()
cpmSetErrorstateWithErrlog =
	get project >>- \p.
	get contents >>- \c.
	'DM'.foldrWithKey (\k v t -> t >>|- saveFile k (foldr joinWithNewline "" v)) (return ()) c >>|-
	get settings >>- \settings ->
	appWorld (\w. snd ('SP'.callProcess settings.dirCmd ["start","/c",settings.dirCpm,p,"--envs="+++settings.dirIDEEnvs,"1>templog.txt","2>errlog.txt"] Nothing w))
	>>|- readFromFile "templog.txt"
	>>- \(Just errors). 
	set (errors) errorstate
	>>|- return ()

cpmSetErrorstate :: Task ()
cpmSetErrorstate = 		
	get settings >>- \sett.
	get project >>- \p.
	compile sett.dirCpm (projdir p) (projname p) sett.dirIDEEnvs >>|
	readFromFile (errordir sett) >>- \(Just errors).
	set errors errorstate >>|
	return ()
	where
	projname p = dropDirectory p
	projdir p = takeDirectory p 
	errordir sett = (takeDirectory sett.dirCpm) </> "Temp" </> "errors"
	
compile :: String String String String  -> Task ()
compile cpmBin buildDir mainModule env
		= appWorld (\w. snd ('SP'.callProcess cpmBin [mainModule,"--envs="+++env] (Just buildDir) w)) @! ()
	
	
cpmSetErrorstateUsingCmd :: Task ()
cpmSetErrorstateUsingCmd =
	get project >>- \p.
	get contents >>- \c.
	'DM'.foldrWithKey (\k v t -> t >>|- saveFile k (foldr joinWithNewline "" v)) (return ()) c >>|-
	get settings >>- \settings ->
	appWorld (\w. snd ('SP'.callProcess settings.dirCpm [p,"--envs="+++settings.dirIDEEnvs] Nothing w))
	//appWorld (\w. snd ('SP'.callProcess settings.dirCmd ["start","/c",settings.dirCpm,p,"--envs="+++settings.dirIDEEnvs,"1>templog.txt","2>errlog.txt"] Nothing w))
	>>|- readFromFile "templog.txt"
	>>- \(Just errors). 
	set (errors) errorstate
	>>|- return ()


