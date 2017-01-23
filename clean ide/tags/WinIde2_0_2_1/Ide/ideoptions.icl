implementation module ideoptions

import StdFunc
import StdWindow
import StdPStClass
import IdeState
import ioutil

ideOptionsDialog :: !*(PSt *General) -> *(PSt *General)
ideOptionsDialog ps
	# (prefs,ps)	= getPrefs ps
	# (wId,ps)		= openId ps
	# (okId,ps)		= openId ps
	# (tpId,ps)		= openId ps
	# (hpId,ps)		= openId ps
	# (prId,ps)		= openId ps
	# iniSWMark		= toMark prefs.switch_close
	# iniTBMark		= toMark prefs.show_toolbar
	# iniBVMark		= toMark prefs.be_verbose
	# iniTPString	= prefs.reg_prefs.tp_path +++ prefs.reg_prefs.tp_name
	# iniHPString	= prefs.reg_prefs.hp_path +++ prefs.reg_prefs.hp_name
	# iniPRString	= prefs.reg_prefs.pr_path +++ prefs.reg_prefs.pr_name
	# (iniNHIndex,iniCVIndex)
					= case prefs.newline_handling of
						(LeaveAlone NewlineConventionNone)	-> (1,1)
						(LeaveAlone NewlineConventionMac)	-> (1,2)
						(LeaveAlone NewlineConventionUnix)	-> (1,3)
						(LeaveAlone NewlineConventionDos)	-> (1,4)
						(AlwaysUse NewlineConventionNone)	-> (2,1)
						(AlwaysUse NewlineConventionMac)	-> (2,2)
						(AlwaysUse NewlineConventionUnix)	-> (2,3)
						(AlwaysUse NewlineConventionDos)	-> (2,4)
	# iniAWMark		= toMark prefs.altgr_workaround
	# (_,ps)		= openModalDialog Void (ddef iniSWMark iniTBMark iniBVMark iniNHIndex iniCVIndex iniAWMark
						iniTPString iniHPString iniPRString
						tpId hpId prId okId wId) ps
	= ps

ddef
	iniSWMark iniTBMark iniBVMark iniNHIndex iniCVIndex iniAWMark
	iniTPString iniHPString iniPRString
	tpId hpId prId okId wId
	= Dialog "IDE Options"
		(	CheckControl
			[("Switch windows",Nothing,iniSWMark,noLS switchSW)
			,("Show toolbar",Nothing,iniTBMark,noLS switchTB)
			,("Verbose diagnostics",Nothing,iniBVMark,noLS switchBV)
			,("AltGr workaround",Nothing,iniAWMark,noLS switchAW)
			] (Columns 1) []
		:+: TextControl "Newline Handling"
			[ControlPos (Left,zero)]
		:+: RadioControl
			[("Leave existing files alone and use given convention for new files",Nothing,noLS (changeNH LeaveAlone))
			,("Always save using given convention",Nothing,noLS (changeNH AlwaysUse))
			]
			(Columns 1)
			(iniNHIndex)
			[ControlPos (Left,zero)]
		:+: TextControl "Newline Convention"
			[ControlPos (Left,zero)]
		:+: RadioControl
			[("Native",Nothing,noLS (changeCV NewlineConventionNone))
			,("Mac",Nothing,noLS (changeCV NewlineConventionMac))
			,("Unix",Nothing,noLS (changeCV NewlineConventionUnix))
			,("Dos",Nothing,noLS (changeCV NewlineConventionDos))
			]
			(Rows 1)
			(iniCVIndex)
			[ControlPos (Left,zero)]
		:+:	TextControl "Time Profiler:" [ControlPos (Left,zero),ControlWidth (ContentWidth "Theorem Prover:")]
		:+: EditControl iniTPString (PixelWidth 300) 1 [ControlId tpId,ControlSelectState Unable]
		:+: ButtonControl "Select..." [ControlFunction (noLS (setTP tpId))]
		:+: TextControl "Heap Profiler:" [ControlPos (Left,zero),ControlWidth (ContentWidth "Theorem Prover:")]
		:+: EditControl iniHPString (PixelWidth 300) 1 [ControlId hpId,ControlSelectState Unable]
		:+: ButtonControl "Select..." [ControlFunction (noLS (setHP hpId))]
		:+: TextControl "Theorem Prover:" [ControlPos (Left,zero),ControlWidth (ContentWidth "Theorem Prover:")]
		:+: EditControl iniPRString (PixelWidth 300) 1 [ControlId prId,ControlSelectState Unable]
		:+: ButtonControl "Select..." [ControlFunction (noLS (setPR prId))]
		:+: ButtonControl "Clear Registry" [ControlPos (Left,zero),ControlFunction (noLS clearRegistry)]
		:+: ButtonControl "OK" [ControlPos (Right,zero), ControlFunction (noLS (closeWindow wId)), ControlId okId]
		) [WindowClose (noLS (closeWindow wId)), WindowId wId, WindowOk okId, WindowCancel okId]

switchAW ps
	# (prefs,ps)	= getPrefs ps
	# prefs			= {prefs & altgr_workaround = not prefs.altgr_workaround}
	= setPrefs prefs ps

switchSW ps
	# (prefs,ps)	= getPrefs ps
	# prefs			= {prefs & switch_close = not prefs.switch_close}
	= setPrefs prefs ps

switchTB ps
	# (prefs,ps)	= getPrefs ps
	# prefs			= {prefs & show_toolbar = not prefs.show_toolbar}
	= setPrefs prefs ps

switchBV ps
	# (prefs,ps)	= getPrefs ps
	# prefs			= {prefs & be_verbose = not prefs.be_verbose}
	= setPrefs prefs ps

changeNH nh ps
	# (prefs,ps)	= getPrefs ps
	# prefs			= {prefs & newline_handling = case prefs.newline_handling of
						(LeaveAlone conv)	-> nh conv
						(AlwaysUse conv)	-> nh conv
					  }
	= setPrefs prefs ps

changeCV cv ps
	# (prefs,ps)	= getPrefs ps
	# prefs			= {prefs & newline_handling = case prefs.newline_handling of
						(LeaveAlone _)	-> LeaveAlone cv
						(AlwaysUse _)	-> AlwaysUse cv
					  }
	= setPrefs prefs ps

//import dodebug
import first_run, UtilIO, PmPath

setTP tpId ps
	# (prefs,ps)	= getPrefs ps
	# (result,ps)	= selectOutputFile "Select time profiler" "*.exe" ps
	| isNothing result = ps
	# full			= fromJust result
	# name			= GetFileName full
	# short			= case GetShortPathName full of
						(True,short)	-> short
						_				-> full
	# rpath			= GetFilePath short
	# (app_path,ps)	= getStup ps
	# spath			= symAppPath app_path (GetFilePath full)
	# ps			= appPIO (setControlText tpId (spath +++ name)) ps
	# prefs			= {prefs & reg_prefs.tp_name = name, reg_prefs.tp_path = spath}
	# ps			= setPrefs prefs ps
	# (ide_name,ide_path,ide_vers,errs)
					= get_ide_from_registry
	| not (isEmpty errs) || ide_name == "" || ide_path == "" || ide_vers == "" = ps
	# errs			= change_pcl_registry_fun ide_name ide_path name rpath
	| errs == []	= ps
	= ps

setHP hpId ps
	# (prefs,ps)	= getPrefs ps
	# (result,ps)	= selectOutputFile "Select heap profiler" "*.exe" ps
	| isNothing result = ps
	# full			= fromJust result
	# name			= GetFileName full
	# short			= case GetShortPathName full of
						(True,short)	-> short
						_				-> full
	# rpath			= GetFilePath short
	# (app_path,ps)	= getStup ps
	# spath			= symAppPath app_path (GetFilePath full)
	# ps			= appPIO (setControlText hpId (spath +++ name)) ps
	# prefs			= {prefs & reg_prefs.hp_name = name, reg_prefs.hp_path = spath}
	# ps			= setPrefs prefs ps
	# (ide_name,ide_path,ide_vers,errs)
					= get_ide_from_registry
	| not (isEmpty errs) || ide_name == "" || ide_path == "" || ide_vers == "" = ps
	# errs			= change_hcl_registry_fun ide_name ide_path name rpath
	| errs == []	= ps
	= ps

setPR prId ps
	# (prefs,ps)	= getPrefs ps
	# (result,ps)	= selectOutputFile "Select theorem prover" "*.exe" ps
	| isNothing result = ps
	# full			= fromJust result
	# name			= GetFileName full
	# short			= case GetShortPathName full of
						(True,short)	-> short
						_				-> full
	# rpath			= GetFilePath short
	# (app_path,ps)	= getStup ps
	# spath			= symAppPath app_path (GetFilePath full)
	# ps			= appPIO (setControlText prId (spath +++ name)) ps
	# prefs			= {prefs & reg_prefs.pr_name = name, reg_prefs.pr_path = spath}
	= setPrefs prefs ps

clearRegistry ps
	# (errs,ps)		= uninstall ps
	| errs == [] = ps
	# (okId,ps)		= openId ps
	# (dlogId,ps)	= openId ps
	# (_,ps)		= openModalDialog Void (Dialog "Clear Registry errors"
						(	TextControl "Clear Registry produced an error:"	[]
						:+:	TextControl (hd errs) [ControlPos (Left,zero)]
						:+: ButtonControl "OK" [ControlId okId,ControlFunction (noLS (closeWindow dlogId))]) 
						[WindowOk okId,WindowId dlogId]) ps
	= ps
