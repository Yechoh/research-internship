implementation module tools

import StdBool, StdFunc, StdFile, StdPStClass, StdSystem
import ExtNotice, StdPathname
import IdeState, UtilIO, PmPath, projwin

//-- call out to supporting applications...

timepsuf	=: " Time Profile.pcl"
timeparg	=: " -h 4M "

heappsuf	=: " Heap Profile0.hcl"
heapparg	=: " -h 4M "

proofsuf	=: ".prj"
proofarg	=: " "

shoprofun :: !*(PSt General) -> *PSt General
shoprofun ps
	# (prj,ps)		= getProject ps
	# execpath		= PR_GetExecPath prj
	// approximate name mangling done by RTE
	// should still take into account max filename length
	# profpath		= quoted_string (RemoveSuffix` execpath +++ timepsuf)
	# (prefs,ps)	= getPrefs ps

	# pcl_path				= prefs.reg_prefs.tp_path
	# (app_path,ps)			= accFiles GetFullApplicationPath ps
	# app_path				= GetLongPathName app_path
	# pcl_path				= fulAppPath app_path pcl_path
	# pcl_path				= case GetShortPathName pcl_path of
								(True,pcl_path)	-> pcl_path
								_				-> pcl_path

	# timepapp		= quoted_string (pcl_path +++ prefs.reg_prefs.tp_name)
	# sp			= timepapp +++ timeparg +++ profpath
	# stup			= RemoveFilename execpath
	# (ok,ps)		= accFiles (FExists stup) ps
	# stup			= if ok (stup +++ "\\") (applicationpath "")
	# (ok,ps)		= accFiles (LaunchApplication sp stup False) ps
	| not ok
		= openNotice (Notice ["Unable to launch " +++  sp +++ ".",stup] (NoticeButton "OK" id) []) ps
	= ps

shoheapfun :: !*(PSt General) -> *PSt General
shoheapfun ps
	# (prj,ps)		= getProject ps
	# execpath		= PR_GetExecPath prj
	// approximate name mangling done by RTE
	// should still take into account max filename length
	# profpath		= quoted_string (RemoveSuffix` execpath +++ heappsuf)
	# (prefs,ps)	= getPrefs ps

	# pcl_path				= prefs.reg_prefs.hp_path
	# (app_path,ps)			= accFiles GetFullApplicationPath ps
	# app_path				= GetLongPathName app_path
	# pcl_path				= fulAppPath app_path pcl_path
	# pcl_path				= case GetShortPathName pcl_path of
								(True,pcl_path)	-> pcl_path
								_				-> pcl_path

	# heappapp		= quoted_string (pcl_path +++ prefs.reg_prefs.hp_name)
	# sp			= heappapp +++  heapparg +++  profpath
	# stup			= RemoveFilename execpath
	# (ok,ps)		= accFiles (FExists stup) ps
	# stup			= if ok (stup +++ "\\") (applicationpath "")
	# (ok,ps)		= accFiles (LaunchApplication sp stup False) ps
	| not ok
		= openNotice (Notice ["Unable to launch " +++  sp +++ ".",stup] (NoticeButton "OK" id) []) ps
	= ps

provefun :: !*(PSt General) -> *PSt General
provefun ps
	# ps			= pm_save ps	// ensure project file is recent...
	
	# (pathname,ps) = getPath ps
	# pr_path		= quoted_string (RemoveSuffix` pathname +++. proofsuf)
	# (prefs,ps)	= getPrefs ps

	# pcl_path				= prefs.reg_prefs.pr_path
	# (app_path,ps)			= accFiles GetFullApplicationPath ps
	# app_path				= GetLongPathName app_path
	# pcl_path				= fulAppPath app_path pcl_path
	# pcl_path				= case GetShortPathName pcl_path of
								(True,pcl_path)	-> pcl_path
								_				-> pcl_path

	# proofapp		= quoted_string (pcl_path +++ prefs.reg_prefs.pr_name)
	# cps			= proofapp +++ proofarg +++ pr_path
	# stup			= RemoveFilename pathname
	# (ok,ps)		= accFiles (FExists stup) ps
	# stup			= if ok (stup +++ "\\") (applicationpath "")
	# (ok,ps)		= accFiles (LaunchApplication cps stup False) ps
	| not ok
		= openNotice (Notice ["Unable to launch " +++  cps +++ "."] (NoticeButton "OK" id) []) ps
	= ps

