implementation module projmen

import StdArray,StdFunc,StdMisc, StdTuple
import StdFileSelect,StdMenu,StdMenuElement,StdPStClass,StdReceiver
import ExtNotice
import IdeState, EdClient, PmProject, UtilStrictLists, PmFiles, PmPath
import projwin, targetui, ioutil, edfiles
import UtilNewlinesFile
import ExtListBox

//--

pm_menu_add :: .Pathname !*(PSt *General) -> *PSt *General;
pm_menu_add path ps
	# (mPrRecId,ps)					= getPMR ps
	# ((err,rep),ps)				= syncSend2 mPrRecId (Add path) ps
	| err <> SendOk || isNothing rep
		= ps
	| length (fromJust rep) <> 1
		= ps
	# ({projIds,mn_sav,mn_sva,md_cmp,md_chk,md_gen,md_cst,md_est},ps)
		= getMenuIds ps
	// do Project menu...
	# (prefs,ps)					= getPrefs ps
	# projIds = case prefs.enable_prover of
					True	-> removeAt 9 projIds
					False	-> removeAt 8 (removeAt 9 projIds)
	# ps		= appPIO (enableMenuElements projIds) ps
	// do Module menu...
	# moduleIds = [mn_sav,mn_sva,md_cmp,md_chk,md_gen,md_cst,md_est]
	# ps		= setModuleIds moduleIds ps
	// if project window or edit window active then enable moduleIds...
	# (wId,ps)	= accPIO getActiveWindow ps
	| isNothing wId
		= ps
	# wId			= fromJust wId
	# (pwId,ps)		= getPWW ps
	| pwId == wId
		# projModuleIds = removeMembers moduleIds [md_cmp,md_chk,md_gen]
		= appPIO (enableMenuElements projModuleIds) ps
	# (isEW,ps)		= isEditWin wId ps
	| isEW
		# (mpath,ps) = message wId getPathName ps
		| isNothing mpath = ps
		# mpath = fromJust mpath
		| ismodule mpath
			= appPIO (enableMenuElements moduleIds) ps
		= ps
	= ps
where
	ismodule s
		| lengths < 4
			= False
		| s%(firsts,lasts) == ".icl"
			= True
//		| s%(firsts,lasts) == ".dcl"
//			= True
		= False
	where
		lengths = size s
		firsts = lengths - 4
		lasts = dec lengths

pm_menu_rem :: !*(PSt *General) -> *PSt *General;
pm_menu_rem ps
	/*
	Show dialog with listbox of projects ---> remove...
	*/
	# (mPrRecId,ps)	= getPMR ps
	# ((err,projs),ps)	= syncSend2 mPrRecId (Get) ps
	| err <> SendOk || isNothing projs
		= ps
	# projs			= fromJust projs
	# (dlogId,ps)	= openId ps
	# (okId,ps)		= openId ps
	# (cancelId,ps)	= openId ps
	# (lbId,ps)		= openExtListBoxId ps
	# (_,ps)		= openModalDialog dloc (ddef dlogId okId cancelId lbId projs) ps
	= ps
where
	buttonWidth	= ContentWidth "Cancel"
	dloc = undef
	do_remove lbId ps
		# ((ok,sel),ps)	= getExtListBoxSelection lbId ps 
		| not ok = ps
		# (sels,seli)	= unzip sel
		# ps			= closeExtListBoxItems lbId seli ps
		# (mPrRecId,ps)	= getPMR ps
		# (_,ps)		= seqList (map (\p->syncSend2 mPrRecId (Rem p)) sels) ps
		# ((err,rep),ps)= syncSend2 mPrRecId Get ps
		| err <> SendOk || isNothing rep
			= ps
		| isEmpty (fromJust rep)
			# ({projIds},ps)	= getMenuIds ps
			# (prefs,ps)		= getPrefs ps
			# projIds = case prefs.enable_prover of
							True	-> removeAt 9 projIds
							False	-> removeAt 8 (removeAt 9 projIds)
			// if projwin or editwin active disable moduleIds...
			// not necessary here because only called from active dialog...
			# ps = setModuleIds [] ps
			= appPIO (disableMenuElements projIds) ps
		= ps
		
	ddef dlogId okId cancelId lbId projs =
			Dialog "Remove..."
			(	TextControl "Remove project from project list" []
			:+: ExtListBoxControl [(p,id,id)\\ p <- projs] [] (\_ ps -> ps) lbId
				[ ControlPos (Left,zero)
				, ControlViewSize {w=300,h=200}
				]
			:+: ButtonControl "Remove"
				[ ControlFunction (noLS (do_remove lbId))
				, ControlPos (Left,zero)
				, ControlWidth buttonWidth
				]
			:+:	ButtonControl "Cancel"	// doesn't cancel...
				[ ControlId cancelId
				, ControlFunction (noLS (closeWindow dlogId))
				, ControlWidth buttonWidth
				]
			:+:	ButtonControl "Ok"
				[ ControlId okId
				, ControlFunction (noLS (closeWindow dlogId))
				, ControlWidth buttonWidth
				]
			)
			[ WindowId dlogId
			, WindowOk okId
			, WindowCancel cancelId
			, WindowClose (noLS (closeWindow dlogId))
			, WindowInitActive okId
			]

ProjListMenu :: Id Id (R2Id PLMMessage PLMReply) -> NewLS (:+: .RadioMenu (Receiver2 PLMMessage PLMReply)) .a *(PSt *General)
ProjListMenu mProjectId mPrListId mPrRecId =
	{newDef =
	(	RadioMenu [] 0 [MenuId mPrListId]
	:+: Receiver2 mPrRecId pm_menu_set []
	)
	,newLS = []
	}
where
	findIndex s l = findIndex` 1 l
	where
		findIndex` _ [] = 0
		findIndex` i [h:t]
			| s == h = i
			= findIndex` (inc i) t

	projectListItem fs =
		(RemovePath fs,Nothing,Nothing,(snd o pm_switch fs))

	pm_menu_set (Add path) (ls,ps)
		# idx = findIndex path ls
		| idx <> 0
			// activate...
			# ps = appPIO (selectRadioMenuIndexItem mPrListId idx) ps
			= (ls,(ls,ps))
		# (err,ps) = accPIO (openRadioMenuItems mPrListId 0 [projectListItem path]) ps
		| err <> NoError
			# ps	= openNotice (Notice ["Unable to update project menu."] (NoticeButton "OK" id) []) ps
			= (ls,(ls,ps))
		# ps = appPIO (selectRadioMenuIndexItem mPrListId 1) ps
		# res = [path:ls]
		= (res,(res,ps))
	pm_menu_set (Rem path) (ls,ps)
		# idx = findIndex path ls
		| idx == 0
			= (ls,(ls,ps))
		# (mnu,ps)	= accPIO (getMenu mProjectId) ps
		| isNothing mnu
			= (ls,(ls,ps))
		# mnu = fromJust mnu
		# (mix,_)	= getSelectedRadioMenuItem mPrListId mnu
		| idx == mix
			// Disallow removing currently active project...
			//# ps = appPIO (selectRadioMenuIndexItem mPrListId 0) ps
			//# ps = appPIO (closeRadioMenuIndexElements mPrListId [idx]) ps
			= (ls,(ls,ps))
		# ps = appPIO (closeRadioMenuIndexElements mPrListId [idx]) ps
		= (ls,(ls,ps))
	pm_menu_set Get (ls,ps)
		= (ls,(ls,ps))

pm_new :: !*(PSt *General) -> *PSt *General;
pm_new ps
	#	(path,ps)		= sendToActiveWindow msgGetPathName ps
	| isNothing path
		// error message no new project without active main module
		= okNotice ["Unable to create new project.","There is no active module window."] ps
	# path				= fromJust path
	# projectpath		= MakeProjectPathname path
	# (mpath,ps)		= selectOutputFile "New Project..." projectpath ps
	| isNothing mpath
		= ps
	# projectpath		= fromJust mpath
	# (_,ps)			= close_all_project_windows (pm_new` path projectpath) ps
	= ps

pm_new` path projectpath ps
	# ps				= pm_shut ps	// just in case
	#	(prefs,ps)		= getPrefs ps
		edo				= {newlines=HostNativeNewlineConvention}
		eo				= {eo = edo, pos_size = DefWindowPos_and_Size}
		co				= prefs.compopts	//DefCompilerOptions
		go				= prefs.cgenopts	//DefCodeGenOptions
		lo				= prefs.linkopts	//DefaultLinkOptions
		ao				= prefs.applopts	//DefApplicationOptions
		project			= PR_NewProject path eo co go ao Nil lo
		ps				= appProject (const project) ps
		ps				= setPath projectpath ps
		ps				= pm_set_window_title projectpath ps
		(tg,ps)			= getTargetName ps
		ps				= setProjectTarget tg ps
		ps				= pm_update_project_window ps
		ps				= pm_save ps
	# ps				= pm_open_path projectpath ps
//	# ps				= pm_menu_add projectpath ps
	= (True,ps)

pm_open :: !*(PSt *General) -> *PSt *General;
pm_open ps
	# (fs,ps)			= selectInputFile ps
	| isNothing fs
		= ps
	# pathname			= fromJust fs
	# name				= RemovePath pathname
	# projectfile		= IsPrjPathname pathname
	| not projectfile
		= okNotice ["The file \"" +++  name +++ "\" is not a project file."] ps
	# ps				= pm_open_path pathname ps
	= ps

pm_open_path :: !.String !*(PSt *General) -> *PSt *General;
pm_open_path pathName ps
	# (ok,ps)		= pm_switch pathName ps
	| not ok
		= ps
	# (mPHrecId,ps)	= getPHI ps
	# (_,ps)		= syncSend2 mPHrecId pathName ps
	= ps
/*
pm_open_cont :: !.Pathname !*(PSt *General) -> *PSt *General;
pm_open_cont file ps
	# ps = pm_switch file ps
	= pm_menu_add file ps
*/
pm_switch :: !.Pathname !*(PSt *General) -> (!Bool,!*PSt *General);
pm_switch pathname ps
	# (oldpath,ps)	= getPath ps
	# (ok,ps)		= close_all_project_windows (pm_switch` pathname) ps
	| not ok && oldpath <> ""
		# (_,ps)	= pm_actual_open oldpath ps
		# ps		= pm_menu_add oldpath ps
		= (ok,ps)
	# ps			= pm_menu_add pathname ps
	= (ok,ps)

pm_switch` pathname ps
	# ps = pm_shut ps
	= pm_actual_open pathname ps

pm_actual_open :: .Pathname !*(PSt *General) -> (!Bool,!*PSt *General);
pm_actual_open pathname ps
	# (startupdir,ps)		= getStup ps
	# ((project,ok,err),ps)	= accFiles (ReadProjectFile pathname startupdir) ps
	| not ok
		= (ok,okNotice [err] ps)
	# ps					= appProject (const project) ps
	# ps					= setPath pathname ps
//	# name					= RemovePath pathname
	# ps					= pm_set_window_title pathname ps
	# ps					= selectProjectTarget getTargets ps
	# ps					= pm_update_project_window ps
	# (prefs,ps)			= getPrefs ps
	| not prefs.switch_close
		= (ok,ps)
	// loop through modules and open those that need to be opened...
	# paths					= PR_GetOpenModulenames project
	# ps					= tryopen paths ps
	// fixup menus
	= (ok,ps)
where
	tryopen Nil ps = ps
	tryopen (h :! t) ps
		# ps	= ed_open_path h ps
		= tryopen t ps
	
pm_shut :: !*(PSt *General) -> *PSt *General;
pm_shut ps
	# (lbId,ps)	= getPWI ps
	# ps		= pm_save ps
	# ps		= appProject (const PR_InitProject) ps
	# ps		= closeAllExtListBoxItems lbId ps
	# ps		= setPath "" ps
	// fixup menus
	= ps

:: AskReply = Yes | No | Cancel
:: ErrorReply = ECancel | EClose | ELeave

close_all_project_windows cont ps
	# (prefs,ps)	= getPrefs ps
	| not prefs.switch_close
		= cont ps
	# (windows,ps)	= accPIO getWindowsStack ps
	# (project,ps)	= accProject (\l->(l,l)) ps
	# modules		= PR_GetModuleStuff project
	# modules		= massage (StrictListToList modules)
	= doall windows modules ps
where
	massage [] = []
	massage [(dm,dp,im,ip):r] = [MakeFullPathname dp dm, MakeFullPathname ip im: massage r]
	
	doall [] _ ps
		= cont ps
	doall [win:res] modules ps
		# (ns,ps)		= message win msgGetNeedSave ps
		| isNothing ns
			= doall res modules ps
		# needsave		= fromJust ns
		# (mr,ps)		= message win msgGetPathName ps
		| isNothing mr
			= doall res modules ps
		# pathname		= fromJust mr
		| not (isMember pathname modules)
			= doall res modules ps
		| not needsave
			= doall res modules (ed_common_close False win ps)
		# texts			= ["Save changes to","\""+++RemovePath pathname+++"\"","before closing?"]
		= ask texts win res ps
	where
		save_cont win sId res modules ps
			# (reply,ps)= message win msgSave ps		//=> ADD ERROR CHECKING HERE!
			| isNothing reply
				= doall res modules ps					// shouldn't be possible...
			# error = fromJust reply
			| isJust error
				# (sId,ps)	= openId ps
				# (okId,ps)	= openId ps
				# (cnId,ps)	= openId ps
				# (ret,ps) = openModalDialog ECancel (edef ["Save failed:",fromJust error] sId okId cnId) ps
			= case ret of
				(NoError,Just ret) -> case ret of
					ECancel	-> (False,ps)
					EClose	-> doall res modules (ed_common_close False win ps)
					ELeave	-> doall res modules ps
				_ -> (False,ps)
			# ps		= ed_common_close False win ps
			= doall res modules ps
		buttonWidth = ContentWidth "Cancel"

		ask :: [String] !Id [Id] !*(PSt *General) -> (!Bool,!*PSt *General);
		ask texts win res ps
			# (sId,ps)	= openId ps
			# (okId,ps)	= openId ps
			# (cnId,ps)	= openId ps
			# (ret,ps)	= openModalDialog Cancel (sdef sId okId cnId) ps
			= case ret of
				(NoError,Just ret) -> case ret of
					Cancel	-> (False,ps)
					No		-> doall res modules (ed_common_close False win ps)
					Yes		-> save_cont win sId res modules ps
				_ -> (False,ps)
				
		where
			sdef sId okId cnId = Dialog ""
					(	ListLS [TextControl txt [ControlPos (Left,zero)]
						\\ txt <- texts
						]
					:+: ButtonControl "Cancel"
						[ ControlPos (Left,zero)
						, ControlFunction (\(ls,ps)->(Cancel,closeWindow sId ps))
						, ControlWidth buttonWidth
						, ControlId cnId
						]	// no more
					:+: ButtonControl "No"
						[ ControlFunction (\(ls,ps)->(No,closeWindow sId ps))
						, ControlWidth buttonWidth
						]	// not this one but continue with rest
					:+: ButtonControl "Yes"
						[ ControlId okId
						, ControlFunction (\(ls,ps)->(Cancel,closeWindow sId ps))
						, ControlWidth buttonWidth
						]	// do this one and continue
					)
					[ WindowOk okId
					, WindowCancel cnId
					, WindowId sId
					]
		edef texts sId okId cnId = Dialog ""
			(	ListLS [TextControl txt [ControlPos (Left,zero)]
				\\ txt <- texts
				]
			:+: ButtonControl "Cancel"
				[ ControlPos (Left,zero)
				, ControlFunction (\(ls,ps)->(ECancel,closeWindow sId ps))
				, ControlWidth buttonWidth
				, ControlId cnId
				]	// no more
			:+: ButtonControl "Close"
				[ ControlFunction (\(ls,ps)->(EClose,closeWindow sId ps))
				, ControlWidth buttonWidth
				]	// not this one but continue with rest
			:+: ButtonControl "Leave"
				[ ControlId okId
				, ControlFunction (\(ls,ps)->(ELeave,closeWindow sId ps))
				, ControlWidth buttonWidth
				]	// do this one and continue
			)
			[ WindowOk okId
			, WindowCancel cnId
			, WindowId sId
			]

