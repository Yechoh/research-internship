implementation module targetui

import StdEnum, StdFunc, StdMisc, StdTuple, StdOrdList
import StdFileSelect, StdMenu, StdMenuElement, StdPStClass, StdSystem
import ExtNotice
import IdeState
import ioutil, tabcontrol
import UtilStrictLists, PmPath

/*
	? add 'New' to edit list dlog
	? add 'Copy' to edit list dlog
	? remove 'New...' menu entry
	? add 'error' checking for space in cocl/cg path and subdir in linker paths...
*/

setProjectTarget :: !String !(PSt *General) -> PSt *General
setProjectTarget name ps
	# ps	= appProject (\project -> PR_SetTarget name project) ps
	= ps

getTargetName :: !(PSt *General) -> (String,PSt *General)
getTargetName ps
	# (tgs,ps) = getTargets ps
	# (cur,ps) = getCurrentTarget ps
	= ((tgs!!cur).target_name,ps)

selectProjectTarget :: !(*(PSt *General) -> *([.Target],*(PSt *General))) !*(PSt *General) -> *(PSt *General)
selectProjectTarget getTs ps
	# (name,ps)	= accProject (\p->(PR_GetTarget p,p)) ps
	# (ts,ps)	= getTs ps
	# idx		= findIndex 0 name ts
	| isNothing idx
		| isEmpty ts
			= okNotice ["Unknown environment: "+++name,"No other environments available,","please define one."] ps
		# defname = (ts!!0).target_name
		# ps = appProject (PR_SetTarget defname) ps
		# ps = okTimedNotice ["Unknown environment: "+++name] (ticksPerSecond*4) ps
		= selectProjectTarget getTs ps
	# idx		= fromJust idx
	# ps		= setCurrentTarget idx ps
	# {target_name}	= ts!!idx
	# ps		= setProjectTarget target_name ps
	# ((_,eTargetId),ps) = getTargetIds ps
	# ps		= appPIO ( (selectRadioMenuIndexItem eTargetId (inc idx))) ps
	= ps
where
	findIndex x name [] = Nothing
	findIndex x name [t=:{target_name=n}:ns]
		| n == name = Just x
		= findIndex (inc x) name ns

//--
	
fixAppPaths stup target=:{target_path = path, target_libs = libs, target_objs=objs}
	= {target & target_path = path`, target_libs = libs`, target_objs=objs`}
where
	path` = fulAppPaths stup path
	libs` = fulAppPaths stup libs
	objs` = fulAppPaths stup objs

//--

targetToMenuEntry t=:{target_name}
	= (t.target_name, Nothing, Nothing, noLS (setProjectTarget target_name o setCurrentTarget` t))

targetToMenuEntry` t=:{target_name}
	= (t.target_name, Nothing, Nothing, (setProjectTarget target_name o setCurrentTarget` t))

targetMenu :: !String [.Target] Id Id (*(PSt *General) -> *([.Target],*(PSt *General))) ([Target] -> .(*(PSt *General) -> *(PSt *General))) -> Menu (:+: .MenuItem (:+: .MenuItem (:+: .MenuItem (:+: .MenuSeparator .RadioMenu)))) .a *(PSt *General);
targetMenu envspath targets mTargetId eTargetId getTargets setTargets
	= Menu "E&nvironment"
		(	MenuItem "&Edit Current..."		[MenuFunction (noLS editfun)]
		:+: MenuItem "&New ..."				[MenuFunction addtfun]
		:+: MenuItem "Edit &List..."		[MenuFunction (noLS remtfun)]
		:+:	MenuSeparator []
		:+: RadioMenu
			[ targetToMenuEntry t
			\\ t <- targets
			] 1 [MenuId eTargetId]
		)
		[ MenuId mTargetId
		]
where
	editfun ps
		# ps			= editTargets getTargets setTargets ps
		# (targets,ps)	= getTargets ps
		# (ok,ps)		= saveEnvironments envspath targets ps
		| not ok
			#	ps	= okNotice ["Unable to save environments settings."] ps
			= ps
		= ps
	remtfun ps
		# (targets,ps)	= getTargets ps
		# (lbId,ps)		= openExtListBoxId ps
		# (dId,ps)		= openId ps
		# (okId,ps)		= openId ps
		# (cancelId,ps)	= openId ps
		# ddef			=	ExtListBoxControl
							[toItem t \\ t <- targets]	// contents = targets
							[1]							// initial selection = first item
							(\_ ps -> ps)
							lbId
							[ControlViewSize {w=200,h=100}]
						:+: ButtonControl "Edit..."
							[ControlFunction (editfun` lbId dId)
							,ControlWidth (ContentWidth "Move Down")
							]
						:+:	ButtonControl "Remove"
							[ControlFunction (remfun lbId dId)
							,ControlWidth (ContentWidth "Move Down")
							,ControlPos (BelowPrev,zero)
							]
						:+:	ButtonControl "Copy..."
							[ControlFunction (copyfun lbId dId)
							,ControlWidth (ContentWidth "Move Down")
							,ControlPos (BelowPrev,zero)
							]
						:+:	ButtonControl "Rename..."
							[ControlFunction (renamefun lbId dId)
							,ControlWidth (ContentWidth "Move Down")
							,ControlPos (BelowPrev,zero)
							]
						:+: ButtonControl "Move Up"
							[ControlFunction (upfun lbId)
							,ControlWidth (ContentWidth "Move Down")
							,ControlPos (BelowPrev,zero)]
						:+: ButtonControl "Move Down"
							[ControlFunction (dnfun lbId)
							,ControlWidth (ContentWidth "Move Down")
							,ControlPos (BelowPrev,zero)]
						:+:	ButtonControl "OK"
							[ ControlPos (Right,zero)
							, ControlFunction (okfun lbId dId (length targets))
							, ControlWidth (ContentWidth "Cancel")
							, ControlId okId
							]
						:+:	ButtonControl "Cancel"
							[ControlPos (LeftOfPrev,zero)
							,ControlFunction (noLS (cancelfun dId))
							,ControlId cancelId
							]
		# datt			= [WindowId dId,WindowOk okId, WindowCancel cancelId,WindowClose (noLS (cancelfun dId))]
		# (_,ps)		= openModalDialog targets (Dialog "Edit Environments List" ddef datt) ps
		= ps
	where
		upfun lbId (targets,ps)
			# (_,(targets,ps)) = upSelItem lbId (targets,ps)
			// adjust radio menu...
			= (targets,ps)
		dnfun lbId (targets,ps)
			# (_,(targets,ps)) = dnSelItem lbId (targets,ps)
			// adjust radio menu...
			= (targets,ps)
		toItem {target_name} = (target_name,id,id)
		editfun` lbId dId (targets,ps)
			# ((ok,sel),ps)	= getExtListBoxSelection lbId ps
			| not ok
				= (targets,ps)
			# sel			= map snd sel
			| isEmpty sel
				= (targets,ps)
			# sel = dec (hd sel)

			# (savetargets,ps) = getTargets ps
			# (savecurrent,ps) = getCurrentTarget ps

			# ps = setTargets targets ps
			# ps = setCurrentTarget sel ps

			# ps = editTargets getTargets setTargets ps

			# (targets,ps) = getTargets ps
			# ps = setTargets savetargets ps
			# ps = setCurrentTarget savecurrent ps
			= (targets,ps)
		remfun lbId dId (targets,ps)
			# ((ok,sel),ps)	= getExtListBoxSelection lbId ps
			| not ok
				= (targets,ps)
			# sel			= map snd sel
			# ps			= closeExtListBoxItems lbId sel ps
			# targets		= remove sel targets
			= (targets,ps)
		where
			remove is ts = remove` 1 (sort is) ts

			remove` _ [] ts = ts
			remove` x [i:is] [t:ts]
				| x > i = abort "target:remove targets: index out of range"
				| x == i = remove` (inc x) is ts
				= [t:remove` (inc x) [i:is] ts]
			remove` _ _ _ = abort "fatal error in remove`"
		copyfun lbId dId (targets,ps)
			# ((ok,sel),ps)	= getExtListBoxSelection lbId ps
			| not ok || isEmpty sel
				= (targets,ps)
			# sel = snd (hd sel)
			# target = targets!!(sel - 1)
			= newNameDialog target.target_name (copycont sel target lbId) (targets,ps)
		copycont sel target lbId target_name (targets,ps)
			# target = {target & target_name = target_name}
			# targets = insertAt (sel-1) target targets
			# ps	= openExtListBoxItems lbId sel [toItem target] ps
			= (targets,ps)
		renamefun lbId dId (targets,ps)
			# ((ok,sel),ps)	= getExtListBoxSelection lbId ps
			| not ok || isEmpty sel
				= (targets,ps)
			# sel = snd (hd sel)
			# target = targets!!(sel - 1)
			= newNameDialog target.target_name (renamecont sel target lbId) (targets,ps)
		renamecont sel target lbId target_name (targets,ps)
			# target = {target & target_name = target_name}
			# targets = updateAt (sel-1) target targets
			# ps	= closeExtListBoxItems lbId [sel] ps
			# ps	= openExtListBoxItems lbId sel [toItem target] ps
			= (targets,ps)
		okfun lbId dId last (targets,ps)
			# (ok,ps)		= saveEnvironments envspath targets ps
			| not ok
				#	ps	= okNotice ["Unable to save environments settings."] ps
				= (targets,ps)
			# ps			= appPIO (closeRadioMenuIndexElements eTargetId [1..last]) ps
			# (err,ps)		= accPIO (openRadioMenuItems eTargetId 1
								[targetToMenuEntry` t
								\\ t <- targets
								]) ps
			
			# ps			= setTargets targets ps
			# ps			= closeWindow dId ps
			= (targets,ps)
		cancelfun dId ps
			= closeWindow dId ps
	addtfun (ls,ps)
		= newNameDialog "new environment" cont (ls,ps)
	where
		cont newname (ls,ps)
			# newtarget		= {t_StdEnv & target_name = newname}
			# (app_path,ps)	= getStup ps
			# newtarget		= fixAppPaths app_path newtarget
			# (targets,ps)	= getTargets ps
			# targets		= targets++[newtarget]
			# ps			= setTargets targets ps
			# newindex		= length targets
			# (err,ps)		= accPIO (openRadioMenuItems eTargetId newindex [targetToMenuEntry` newtarget]) ps
			| err <> NoError
				= abort "targetui.icl: strange error adding target"
			# ps			= setProjectTarget newname ps
			# ps			= selectProjectTarget getTargets ps
			# ps			= editTargets getTargets setTargets ps
			# (ok,ps)		= saveEnvironments envspath targets ps
			| not ok
				# ps		= okNotice ["Unable to save environments settings."] ps
				= (ls,ps)
			= (ls,ps)

//--

newNameDialog ininame cont (ls,ps)
	# (editId,ps)	= openId ps
	# (windId,ps)	= openId ps
	# (okId,ps)		= openId ps
	# (cancelId,ps)		= openId ps
	# ((err,en),ps)	= openModalDialog Nothing
						(Dialog "Environment Name"
						(	EditControl ininame (PixelWidth 150) 1
							[ControlId editId]
						:+: ButtonControl "OK"
							[ControlPos (Right,zero),ControlFunction (okfun editId windId),ControlId okId]
						:+: ButtonControl "Cancel"
							[ControlPos (LeftOfPrev,zero),ControlFunction (cancelfun windId),ControlId cancelId]
						)
						[ WindowId windId,WindowClose (cancelfun windId)
						, WindowOk okId
						, WindowCancel cancelId
						]
						) ps
	| err <> NoError
		= (ls,ps)
	| isNothing en
		= (ls,ps)
	| isNothing (fromJust en)
		= (ls,ps)
	= cont (fromJust (fromJust en)) (ls,ps)
where
	okfun editId windId (_,ps)
		# (wstate,ps)		= accPIO (getWindow windId) ps
		| isNothing wstate
			= (Nothing, closeWindow windId ps)
		# wstate			= fromJust wstate
		# (_,maybename)	= getControlText editId wstate
		= (maybename,closeWindow windId ps)
	cancelfun windId (_,ps)
		= (Nothing,closeWindow windId ps)

//--

setCurrentTarget` t ps
	# (ts,ps) = getTargets ps
	# x = find t ts
	| x < 0
		// silly solution
		= setCurrentTarget 0 ps
	# ps = setCurrentTarget x ps
	= ps
where
	find t ts = find` 0 ts
	where
		find` x [] = (-1)
		find` x [h:ts]
			| t.target_name == h.target_name = x
			= find` (inc x) ts

//--- Environment settings dialog

buttonWidth = ContentWidth "Append..."

editTargets getTs setTs ps
	# (ap,ps) 	= getStup ps
	# (pp,ps) 	= getPath ps
	# pp		= RemoveFilename pp
	# (ts,ps)	= getTs ps
	# (ct,ps)	= getCurrentTarget ps
	# ps		= envDialog ap pp ts ct getTs setTs ps
	= ps

:: LS ps =
	{ tg	:: !Target
	, full	:: !Bool
	, ap	:: !String
	, pp	:: !String
	
	, c1id	:: !Id
	, c2id	:: !Id
	, c3id	:: !Id
	, c4id	:: !Id
	
	, lbsllId	:: !ExtListBoxId ps
	, lblibId	:: !ExtListBoxId ps
	, lbobjId	:: !ExtListBoxId ps
	, lbpadId	:: !ExtListBoxId ps
	}

envDialog ap pp ts ct getTs setTs ps
	# (wid,ps)	= openId ps
	# (okId,ps)	= openId ps
	# (cancelId,ps)	= openId ps
	# (r1id,ps) = openId ps
	# (r2id,ps) = openId ps
	# (r3id,ps) = openId ps
	# (r4id,ps) = openId ps
	# (siz,ps)	= controlSize (LayoutControl
					(	ButtonControl "Append..." [ControlWidth buttonWidth]
					:+: ButtonControl "Remove" [ControlPos (Left,zero),ControlWidth buttonWidth]
					)
					[ ControlPos (Left,zero)
					, ControlHMargin 0 0
					, ControlVMargin 0 0
					])
					True
					Nothing Nothing Nothing ps
	# (lbobjId,ps) = openExtListBoxId ps
	# (lbobj) = ExtListBoxControl
	  				(zip3(StrictListToList(FullPaths inifull ap pp tg.target_objs))(repeat id)(repeat id))
	  				[] 						// initial selection
	  				(\sel ps->case sel of
	  					[]	-> appPIO (disableControl r4id) ps
	  					_	-> appPIO (enableControl r4id) ps
	  				)						// selection update function
	  				lbobjId
	  				[ControlViewSize {h=siz.Size.h,w=300}]
	# (lblibId,ps) = openExtListBoxId ps
	# (lblib) = ExtListBoxControl
	  				(zip3(StrictListToList(FullPaths inifull ap pp tg.target_libs))(repeat id)(repeat id))
	  				[] // initial selection
	  				(\sel ps->case sel of
	  					[]	-> appPIO (disableControl r2id) ps
	  					_	-> appPIO (enableControl r2id) ps
	  				)						// selection update function
	  				lblibId
	  				[ControlViewSize {h=siz.Size.h,w=300}]
	# (lbpadId,ps) = openExtListBoxId ps
	# (lbpad) = ExtListBoxControl
	  				(zip3(StrictListToList(FullPaths inifull ap pp tg.target_path))(repeat id)(repeat id))
	  				[]						// initial selection
	  				(\sel ps->case sel of
	  					[]	-> appPIO (disableControl r1id) ps
	  					_	-> appPIO (enableControl r1id) ps
	  				)						// selection update function
	  				lbpadId
	  				[ControlViewSize {h=siz.Size.h,w=300}]
	# (lbsllId,ps) = openExtListBoxId ps
	# lbsll = ExtListBoxControl
	  				(zip3(StrictListToList(FullPaths inifull ap pp tg.target_stat))(repeat id)(repeat id))
	  				[] // initial selection
	  				(\sel ps->case sel of
	  					[]	-> appPIO (disableControl r3id) ps
	  					_	-> appPIO (enableControl r3id) ps
	  				)						// selection update function
	  				lbsllId
	  				[ControlViewSize {h=siz.Size.h,w=300}]
	# (compId,ps) = openId ps
	# (cgenId,ps) = openId ps
	# (linkId,ps) = openId ps
	# (dynlId,ps) = openId ps
	# (versId,ps) = openId ps
	# (methId,ps) = openId ps
	# (c1id,ps) = openId ps
	# (c2id,ps) = openId ps
	# (c3id,ps) = openId ps
	# (c4id,ps) = openId ps
	# iniLS =
		{ tg	= tg
		, full	= inifull
		, ap	= ap
		, pp	= pp
		, c1id	= c1id
		, c2id	= c2id
		, c3id	= c3id
		, c4id	= c4id
		, lbpadId	= lbpadId
		, lbobjId	= lbobjId
		, lbsllId	= lbsllId
		, lblibId	= lblibId
		}
	# (_,ps) = openModalDialog iniLS
				(ddef wid okId cancelId lbobj lbobjId lblib lblibId lbpad lbpadId lbsll
				 lbsllId siz compId cgenId linkId dynlId versId methId c1id c2id c3id c4id
				 r1id r2id r3id r4id
				) ps
	= ps
where
	tg = ts!!ct
	inifull = False

	ddef wid okId cancelId lbobj lbobjId lblib lblibId lbpad lbpadId lbsll lbsllId siz compId cgenId linkId dynlId
		versId methId c1id c2id c3id c4id r1id r2id r3id r4id
		= Dialog ("Environment: "+++tg.target_name)
			(title :+: panes :+: buttons`)
			[WindowId wid, WindowOk okId, WindowClose (noLS (closeWindow wid)), WindowCancel cancelId]
	where
		buttons` = buttons wid okId cancelId getTs setTs ct ts compId cgenId linkId dynlId versId methId
		panes = TabControl
					(	Pane "Tools" (toolpane tg compId cgenId linkId dynlId versId methId)
					:+: Pane "Paths" (pathpane inifull lbpad lbpadId c1id r1id)
					:+: Pane "Dynamic Libraries" (dlibpane inifull lblib c2id r2id)
					:+: Pane "Static Libraries" (slibpane inifull lbsll c3id r3id)
					:+: Pane "Object Modules" (objmpane inifull lbobj c4id r4id)
					) [ControlPos (Left,zero)]

buttons wid okId cancelId getTs setTs ct ts compId cgenId linkId dynlId versId methId
	=	ButtonControl "Save"
		[ ControlFunction savefun
		, ControlId okId, ControlPos (Right,zero)
		, ControlWidth width
		]
	:+: ButtonControl "Save As..."
		[ ControlFunction saveasfun
		, ControlPos (LeftOfPrev,zero)
		, ControlWidth width
		]
	:+:	ButtonControl "Cancel"
		[ ControlFunction (noLS (closeWindow wid))
		, ControlPos (LeftOfPrev,zero)
		, ControlWidth width
		, ControlId cancelId
		]
where
	width = ContentWidth "Save As..."

	savefun (ls,ps)
		# (ls,ps)	= commonsave (ls,ps)
		# ps		= setTs (updateAt ct ls.tg ts) ps
		# ps		= setProjectTarget ls.tg.target_name ps
		// possible since we know it's only possible to edit the active environment...
		= (ls,ps)
	saveasfun (ls,ps)
		# (ls,ps)	= commonsave (ls,ps)
		= newNameDialog ls.tg.target_name contSaveAs (ls,ps)
	contSaveAs target_name (ls,ps)
		# ls		= {ls & tg.target_name = target_name}
		# ps		= setTs (ts++[ls.tg]) ps
		# ((_,eTargetId),ps) = getTargetIds ps
		# (err,ps)		= accPIO (openRadioMenuItems eTargetId (1 + length ts) [targetToMenuEntry` ls.tg]) ps
		| err <> NoError
			= abort "targetui.icl: strange error adding target in saveas"
		# ps		= setProjectTarget target_name ps
		// possible since we know it's only possible to edit the active environment...
		# ps		= selectProjectTarget getTs ps
		= (ls,ps)


	commonsave (ls,ps)
		# (wdef,ps) = accPIO (getWindow wid) ps
		| isNothing wdef
			= abort "Fatal error in environment dialog: window has disappeared."
		# wdef		= fromJust wdef
		# [(ok1,comp),(ok2,cgen),(ok3,link),(ok4,dynl),(ok5,vers),(ok6,mnum):_]
					= getControlTexts [compId,cgenId,linkId,dynlId,versId,methId] wdef
		| not (ok1 && ok2 && ok3 && ok4 && ok5 && ok6)
			= abort "Fatal error in environment dialog: controls have disappeared."
		| (isNothing comp) || (isNothing cgen) || (isNothing link) || (isNothing dynl)
		|| (isNothing vers) || (isNothing mnum)
			= abort "Fatal error in environment dialog: controls are empty."
		# comp = fromJust comp
		# cgen = fromJust cgen
		# link = fromJust link
		# dynl = fromJust dynl
		# vers = fromJust vers
		# mnum = fromJust mnum
		# ls =
				{ ls
				& tg.target_comp = comp
				, tg.target_cgen = cgen
				, tg.target_link = link
				, tg.target_dynl = dynl
				, tg.target_vers = toInt vers
				}
		# ls = case ls.tg.target_meth of
				(CompileAsync _)	-> {ls & tg.target_meth = CompileAsync (toInt mnum)}
				_					-> ls
		# ps = closeWindow wid ps
		= (ls,ps)

//--

title
	= TextControl "Environment Options" [ControlPos (Center,zero)]

toolpane tg compId cgenId linkId dynlId versId methId
	=	TextControl "Compiler: " [ControlWidth textWidth]
	:+: EditControl tg.target_comp (PixelWidth 250) 1 [ControlId compId]
	:+: TextControl "Generator: " [ControlPos (Left,zero),ControlWidth textWidth]
	:+: EditControl tg.target_cgen (PixelWidth 250) 1 [ControlId cgenId]
	:+: TextControl "Static Linker: " [ControlPos (Left,zero),ControlWidth textWidth]
	:+: EditControl tg.target_link (PixelWidth 250) 1 [ControlId linkId]
	:+: TextControl "Dynamic Linker: " [ControlPos (Left,zero),ControlWidth textWidth]
	:+: EditControl tg.target_dynl (PixelWidth 250) 1 [ControlId dynlId]
	:+: TextControl "ABC version: " [ControlPos (Left,zero),ControlWidth textWidth]
	:+: EditControl (toString tg.target_vers) (PixelWidth 250) 1 [ControlId versId]
	:+: CheckControl [("Console in IDE",Nothing,toMark tg.target_redc,redcfun)] (Columns 1) [ControlPos (Left,zero)]
	:+: RadioControl
		[("Sync",Nothing,methfun 1)
		,("Async",Nothing,methfun 2)
		,("Pers",Nothing,methfun 3)
		]
		(Columns 1)
		(case tg.target_meth of
			CompileSync			-> 1
			(CompileAsync _)	-> 2
			CompilePers			-> 3
		)
		[ControlPos (Left,zero)]
	:+: EditControl
		(case tg.target_meth of
			(CompileAsync n)	-> toString n
			_					-> "1"
		)
		(PixelWidth 250) 1
		[ControlId methId
		,case tg.target_meth of
			(CompileAsync _)	-> ControlSelectState Able
			_					-> ControlSelectState Unable
		]
where
	textWidth = ContentWidth "Dynamic Linker: "

	redcfun (ls,ps)
		# ls = {ls & tg.target_redc = not ls.tg.target_redc}
		= (ls,ps)
	
	methfun 1 (ls,ps)
		# ps = appPIO (disableControl methId) ps
		# ls = {ls & tg.target_meth = CompileSync}
		= (ls,ps)
	methfun 2 (ls,ps)
		# ps = appPIO (enableControl methId) ps
		# num = 42
		# ls = {ls & tg.target_meth = CompileAsync num}
		= (ls,ps)
	methfun 3 (ls,ps)
		# ps = appPIO (disableControl methId) ps
		# ls = {ls & tg.target_meth = CompilePers}
		= (ls,ps)

pathpane inifull lbpad lbpadId c1id r1id
	=	TextControl "Paths" []
	:+: LayoutControl
		(	ButtonControl "Append..."
			[ ControlFunction	addPath
			, ControlWidth		buttonWidth
			]
		:+: ButtonControl "Remove"
			[ ControlPos		(Left,zero)
			, ControlFunction	removePath
			, ControlWidth		buttonWidth
			, ControlId			r1id
			, ControlSelectState	Unable
			]
		) [ControlPos (Left,zero),ControlHMargin 0 0, ControlVMargin 0 0]
	:+: lbpad
	:+: CheckControl
		[ ("Show Full Names",Nothing,toMark inifull,showFullPaths)
		]
		(Columns 1)
		[ ControlPos (Left,zero)
		, ControlId c1id
		]

dlibpane inifull lblib c2id r2id
	=	TextControl "Dynamic Libraries" []
	:+: LayoutControl
		(	ButtonControl "Append..."
			[ ControlFunction addLibrary
			, ControlWidth buttonWidth
			]
		:+: ButtonControl "Remove"
			[ ControlPos (Left,zero)
			, ControlFunction remLibrary
			, ControlWidth buttonWidth
			, ControlId			r2id
			, ControlSelectState	Unable
			]
		) [ControlPos (Left,zero),ControlHMargin 0 0, ControlVMargin 0 0]
	:+: lblib
	:+: CheckControl
		[ ("Show Full Names",Nothing,toMark inifull,showFullPaths)
		]
		(Columns 1)
		[ ControlPos (Left,zero)
		, ControlId c2id
		]

slibpane inifull lbsll c3id r3id
	= TextControl "Static Libraries" []
	:+: LayoutControl
		(	ButtonControl "Append..."
			[ ControlFunction addStatic
			, ControlWidth buttonWidth
			]
		:+: ButtonControl "Remove"
			[ ControlPos (Left,zero)
			, ControlFunction remStatic
			, ControlWidth buttonWidth
			, ControlId			r3id
			, ControlSelectState	Unable
			]
		) [ControlPos (Left,zero),ControlHMargin 0 0, ControlVMargin 0 0]
	:+: lbsll
	:+: CheckControl
		[ ("Show Full Names",Nothing,toMark inifull,showFullPaths)
		]
		(Columns 1)
		[ ControlPos (Left,zero)
		, ControlId c3id
		]

objmpane inifull lbobj c4id r4id
	=	TextControl "Object Modules" []
	:+: LayoutControl
		(	ButtonControl "Append..."
			[ ControlFunction		addObject
			, ControlWidth			buttonWidth
			]
		:+: ButtonControl "Remove"
			[ ControlPos			(Left,zero)
			, ControlFunction		remObject
			, ControlWidth			buttonWidth
			, ControlId				r4id
			, ControlSelectState	Unable
			]
		) [ControlPos (Left,zero),ControlHMargin 0 0, ControlVMargin 0 0]
	:+: lbobj
	:+: CheckControl
		[ ("Show Full Names",Nothing,toMark inifull,showFullPaths)
		]
		(Columns 1)
		[ ControlPos (Left,zero)
		, ControlId c4id
		]

//--

addPath (ls,ps)
	#	(fs,ps)				= selectDirectory` ps
	| isNothing fs = (ls,ps)
//	#	pathname			= RemoveFilename (fromJust fs)
	#	pathname			= fromJust fs
		ls					= {ls & tg.target_path = Append ls.tg.target_path pathname}
		ps					= appendExtListBoxItems ls.lbpadId (zip3 [FullPath ls.full ls.ap ls.pp pathname](repeat id)(repeat id)) ps
	= (ls,ps)

removePath (ls,ps)
	#	((ok,sel),ps)		= getExtListBoxSelection ls.lbpadId ps
	| not ok || isEmpty sel = (ls,ps)
	#	(pathsel,indexsel)	= unzip sel
		ls					= {ls & tg.target_path = RemoveMembers ls.tg.target_path (ListToStrictList [fulPath ls.ap ls.pp s \\ s <- pathsel])}
		ps					= closeExtListBoxItems ls.lbpadId indexsel ps
		ps					= setExtListBoxSelection ls.lbpadId [] ps
	= (ls,ps)

showFullPaths (ls=:{ap,pp,tg},ps)
	#	full= not ls.full
		ps	= appPIO (setCheckControlMarks [ls.c1id,ls.c2id,ls.c3id,ls.c4id] full) ps
		ps	= closeAllExtListBoxItems ls.lbpadId ps
		ps	= appendExtListBoxItems ls.lbpadId (zip3 (StrictListToList(FullPaths full ap pp tg.target_path))(repeat id)(repeat id)) ps
		ps	= closeAllExtListBoxItems ls.lbobjId ps
		ps	= appendExtListBoxItems ls.lbobjId (zip3 (StrictListToList(FullPaths full ap pp tg.target_objs))(repeat id)(repeat id)) ps
		ps	= closeAllExtListBoxItems ls.lblibId ps
		ps	= appendExtListBoxItems ls.lblibId (zip3 (StrictListToList(FullPaths full ap pp tg.target_libs))(repeat id)(repeat id)) ps
		ps	= closeAllExtListBoxItems ls.lbsllId ps
		ps	= appendExtListBoxItems ls.lbsllId (zip3 (StrictListToList(FullPaths full ap pp tg.target_stat))(repeat id)(repeat id)) ps
	= ({ls & full = full},ps)

addLibrary (ls,ps)
	#	(fs,ps)				= selectInputFile ps
	| isNothing fs = (ls,ps)
	#	pathname			= (fromJust fs)
		ls					= {ls & tg.target_libs = Append ls.tg.target_libs pathname}
		ps					= appendExtListBoxItems ls.lblibId (zip3[FullPath ls.full ls.ap ls.pp pathname](repeat id)(repeat id)) ps
	= (ls,ps)

remLibrary (ls,ps)
	#	((ok,sel),ps)		= getExtListBoxSelection ls.lblibId ps
	| not ok || isEmpty sel = (ls,ps)
	#	(pathsel,indexsel)	= unzip sel
		ls					= {ls & tg.target_libs = RemoveMembers ls.tg.target_libs
									(fulPaths ls.ap ls.pp (ListToStrictList pathsel))}
		ps					= closeExtListBoxItems ls.lblibId indexsel ps
		ps					= setExtListBoxSelection ls.lblibId [] ps
	= (ls,ps)

addStatic (ls,ps)
	#	(fs,ps)				= selectInputFile ps
	| isNothing fs = (ls,ps)
	#	pathname			= (fromJust fs)
		ls					= {ls & tg.target_stat = Append ls.tg.target_stat pathname}
		ps					= appendExtListBoxItems ls.lbsllId (zip3[FullPath ls.full ls.ap ls.pp pathname](repeat id)(repeat id)) ps
	=  (ls,ps)

remStatic (ls,ps)
	#	((ok,sel),ps)		= getExtListBoxSelection ls.lbsllId ps
	| not ok || isEmpty sel = (ls,ps)
	#	(pathsel,indexsel)	= unzip sel
		ls					= {ls & tg.target_stat = RemoveMembers ls.tg.target_stat
									(fulPaths ls.ap ls.pp (ListToStrictList pathsel))}
		ps					= closeExtListBoxItems ls.lbsllId indexsel ps
		ps					= setExtListBoxSelection ls.lbsllId [] ps
	=  (ls,ps)
				
addObject (ls,ps)
	#	(fs,ps)				= selectInputFile ps
	| isNothing fs = (ls,ps)
	#	pathname			=  (fromJust fs)
		ls					= {ls & tg.target_objs = Append ls.tg.target_objs pathname}
		ps					= appendExtListBoxItems ls.lbobjId (zip3[FullPath ls.full ls.ap ls.pp pathname](repeat id)(repeat id)) ps
	= (ls,ps)

remObject (ls,ps)
	#	((ok,sel),ps)		= getExtListBoxSelection ls.lbobjId ps
	| not ok || isEmpty sel = (ls,ps)
	#	(pathsel,indexsel)	= unzip sel
		ls					= {ls & tg.target_objs = RemoveMembers ls.tg.target_objs
									(fulPaths ls.ap ls.pp (ListToStrictList pathsel))}
		ps					= closeExtListBoxItems ls.lbobjId indexsel ps
		ps					= setExtListBoxSelection ls.lbobjId [] ps
	= (ls,ps)

//--

setCheckControlMarks ids full io
	= seq [setCheckControlMark full id \\ id <- ids] io
setCheckControlMark full id io
	= case full of
		True	-> markCheckControlItems id [1] io
		False	-> unmarkCheckControlItems id [1] io

//--

FullPath True _ _ p = p
FullPath False ap pp l = symPath ap pp l

FullPaths True _ _ l = l
FullPaths False ap pp l = symPaths ap pp l