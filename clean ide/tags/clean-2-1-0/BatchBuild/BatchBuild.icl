module BatchBuild

import StdEnv, StdIO
import ArgEnv
import PmDriver
import PmProject
import IdeState
import UtilIO
import PmEnvironment, logfile, set_return_code

Start world
	# (startup,world)			= accFiles GetFullApplicationPath world
	# envspath					= applicationpath EnvsFileName
	# (envs,world)				= openEnvironments startup envspath world
//	| not ok					= wAbort ("Unable to read environments\n") world
	| not path_ok				= wAbort ("BatchBuild\nUse as: 'BatchBuild projectname.prj'\n") world
	# ((proj,ok,err),world)		= accFiles (ReadProjectFile proj_path startup) world
	| not ok || err <> ""		= wAbort ("BatchBuild failed while opening project: "+++.err+++."\n") world
	# (ok,logfile,world)		= openLogfile proj_path world
	| not ok					= wAbort ("BatchBuild failed while opening logfile.\n") world
	# (id1,world)				= openId world
	# (id2,world)				= openId world
	# iniGeneral				= initGeneral True default_compiler_options startup proj_path proj envs logfile id1 id2
	#! world					= startIO NDI iniGeneral pinit [ProcessClose closeProcess] world
	= finish world
where
	commandline					= getCommandLine
	args						= [arg \\ arg <-: commandline]
	default_compiler_options	= DefaultCompilerOptions
	(path_ok,proj_path)			= case args of
				[_,prj]	-> (True,GetLongPathName prj)
				_		-> (False, "")

pinit ps
	#! ps = BringProjectUptoDate False cleanup ps
	= ps
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