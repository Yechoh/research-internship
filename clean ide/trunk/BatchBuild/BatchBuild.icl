module BatchBuild

import StdEnv
import ArgEnv
import PmDriver
import PmProject
import IdeState
from UtilIO import GetFullApplicationPath,GetLongPathName
import PmEnvironment, logfile, set_return_code
from Platform import application_path

Start world
	# commandline				= getCommandLine
	  args						= [arg \\ arg <-: commandline]
	  (path_ok,force_rebuild,proj_path)
			= case args of
				[_,prj]
					-> (True,False,GetLongPathName prj)
				[_,"--force",prj]
					-> (True,True,GetLongPathName prj)
				_
					-> (False,False,"")
	# (startup,world)			= accFiles GetFullApplicationPath world
	# envspath					= application_path EnvsFileName
	# (envs,world)				= openEnvironments startup envspath world
//	| not ok					= wAbort ("Unable to read environments\n") world
	| not path_ok				= wAbort ("BatchBuild\nUse as: 'BatchBuild [--force] projectname.prj'\n") world
	# ((proj,ok,err),world)		= accFiles (ReadProjectFile proj_path startup) world
	| not ok || err <> ""		= wAbort ("BatchBuild failed while opening project: "+++.err+++."\n") world
	# (ok,logfile,world)		= openLogfile proj_path world
	| not ok					= wAbort ("BatchBuild failed while opening logfile.\n") world
	# default_compiler_options	= DefaultCompilerOptions
	# iniGeneral				= initGeneral True default_compiler_options startup proj_path proj envs logfile
	# ps = {ls=iniGeneral,gst_world=world,gst_continue_or_stop=False}
	# {ls,gst_world} = pinit force_rebuild ps
	= finish gst_world

pinit force_rebuild ps
	= BringProjectUptoDate force_rebuild cleanup ps
where
	cleanup exepath bool1 bool2 ps
		= abortLog False "" ps

wAbort message world
//	# (console,world) = stdio world
//	# console = console <<< message
//	# (_,world) = fclose console world
	# stderr		= fwrites message stderr
	# (ok,world)	= fclose stderr world
	# world			= set_return_code_world (-1) world
	= finish world

//finish :: !*World -> String
//finish _ = ""
finish w = w

