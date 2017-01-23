implementation module tools

import StdBool, StdFunc, StdFile, StdPStClass, StdSystem
import ExtNotice, StdPathname
import IdeState, UtilIO

//-- call out to supporting applications...
//-- should make these settable in the ide...
//-- ahhh... problem is then how to inform the change_registry
//-- application of their new location...

shoprofun :: !*(PSt *General) -> *PSt *General
shoprofun ps
	# (prj,ps)		= getProject ps
	# execpath		= PR_GetExecPath prj
	// approximate name mangling done by RTE
	//--> expand pathname...
	// should still take into account max filename length
	# profpath		= (RemoveSuffix` execpath)+++" Time Profile.pcl"
	# applpath		= applicationpath "ShowTimeProfile.exe"
	# sp			= quoted_string applpath +++ " -h 4M " +++ (quoted_string profpath)
	# stup			= RemoveFilename execpath
	# (ok,ps)		= accFiles (FExists stup) ps
	# stup = case ok of
					True  -> stup +++ "\\"
					False -> applicationpath ""
	// check if legal stup...
	# (ok,ps)		= accFiles (LaunchApplication sp stup False) ps
	| not ok
		# ps		= openNotice (Notice ["Unable to launch " +++  sp +++ ".",stup] (NoticeButton "OK" id) []) ps
		= ps
	= ps

shoheapfun :: !*(PSt *General) -> *PSt *General
shoheapfun ps
	# (prj,ps)		= getProject ps
	# execpath		= PR_GetExecPath prj
	// approximate name mangling done by RTE
	// should still take into account max filename length
	# profpath		= (RemoveSuffix` execpath)+++" Heap Profile0.hcl"
	# applpath		= applicationpath "ShowHeapProfile.exe"
	# sp			= quoted_string applpath +++  " -h 4M " +++  (quoted_string profpath)
	# stup			= RemoveFilename execpath
	# (ok,ps)		= accFiles (FExists stup) ps
	# stup = case ok of
					True  -> stup +++ "\\"
					False -> applicationpath ""
	# (ok,ps)		= accFiles (LaunchApplication sp stup False) ps
	| not ok
		#	ps		= openNotice (Notice ["Unable to launch " +++  sp +++ ".",stup] (NoticeButton "OK" id) []) ps
		= ps
	= ps

provefun :: !*(PSt *General) -> *PSt *General
provefun ps
	# (pathname,ps) = getPath ps
	# cps			= quoted_string (applicationpath "CleanProverSystem.exe")+++" "+++quoted_string (RemoveSuffix` pathname +++. ".pr_")
	# stup			= RemoveFilename pathname
	# (ok,ps)		= accFiles (FExists stup) ps
	# stup = case ok of
					True  -> stup +++ "\\"
					False -> applicationpath ""
	# (ok,ps)		= accFiles (LaunchApplication cps stup False) ps
	| not ok
		#	ps		= openNotice (Notice ["Unable to launch " +++  cps +++ "."] (NoticeButton "OK" id) []) ps
		= ps
	= ps

