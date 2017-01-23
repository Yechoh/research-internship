implementation module projwin

import StdArray, StdFunc, StdMisc, StdOrdList, StdTuple
import StdClipboard,StdControlReceiver,StdFileSelect,StdMenuElement,StdPStClass,StdSystem
import ExtListBox, ExtNotice
import EdClient
import PmProject, PmFiles, UtilStrictLists, PmPath
import edfiles, messwin, errwin
import PmDialogues, PmDriver
import PmCleanSystem
import ProverOptions
import flextextcontrol
import ioutil, morecontrols, colorpickcontrol
import projmen, menubar, colourclip
from IDE import OpenModule
import Platform, IdePlatform

//-- Project Window Options...

:: PWO =
	{ fn	:: !String		// font name
	, fs	:: !Int			// font size
	, fc	:: !Colour		// foreground colour
	, bc	:: !Colour		// background colour
	, hc	:: !Colour		// header colour
	, cr	:: !AC			// active colour control in PWO dialogue
	, shift	:: !Bool		// open dcl/icl on dbl-click
	}

:: AC = HC | FC | BC

instance == AC
where
	(==) HC HC = True
	(==) FC FC = True
	(==) BC BC = True
	(==) _ _ = False
	
projwinOptions :: !*(PSt *General) -> *PSt *General;
projwinOptions ps
    # (dialogId,ps)	= openId ps
    # (okId,ps)		= openId ps
    # (cancelId,ps)	= openId ps
	# (hcid,ps)		= openId ps
	# (fcid,ps)		= openId ps
	# (bcid,ps)		= openId ps
	# (rgbid,ps)	= openRGBId ps
	# (lsid,ps)		= openRId ps
	# (fontNames, ps) = accPIO (accScreenPicture getFontNames) ps
	# fontSizes		= [7,8,9,10,11,12]
	# (lbId,ps)		= getPWI ps
	# (pen,ps)		= getExtListBoxPen lbId ps
	| isNothing pen
		= ps
	# pen			= fromJust pen
	# fdef			= getPenAttributeFont pen
	# inifn			= fdef.fName
	# inifs			= fdef.fSize
	# (prefs,ps)	= getPrefs ps
	# inihc			= prefs.prj_prefs.proj_topc
	# inifc			= prefs.prj_prefs.proj_forc
	# inibc			= prefs.prj_prefs.proj_bacc
	# inishift		= prefs.prj_prefs.proj_shft
	# inistate		= {fn = inifn, fs = inifs, fc = inifc, bc = inibc, hc = inihc, cr = HC, shift = inishift}
    # controls		=
    	(	LayoutControl
		(	FontNameSizeControl inifn inifs fontNames fontSizes fontfun sizefun [ left ]
		:+: RGBColourPickControl` rgbid inihc hcid (Just (Left,zero))
		) []

		:+: LayoutControl
		(	TextControl "Header:" [ left , ControlWidth (ContentWidth "Background: ")]
		:+: ColourBoxControl` (toRGBColour inihc) hcid (mfilter,mfun rgbid lsid hcid fcid bcid HC) Nothing
		:+:	TextControl "Text:" [ left , ControlWidth (ContentWidth "Background: ")]
		:+: ColourBoxControl` (toRGBColour inifc) fcid (mfilter,mfun rgbid lsid hcid fcid bcid FC) Nothing
		:+:	TextControl "Background:" [ left , ControlWidth (ContentWidth "Background: ")]
		:+: ColourBoxControl` (toRGBColour inibc) bcid (mfilter,mfun rgbid lsid hcid fcid bcid BC) Nothing
		) []

	   	:+: RadioControl
	   		[("Open .dcl on double-click",Nothing,\(ls,ps)->({ls & shift = True},ps))
	   		,("Open .icl on double-click",Nothing,\(ls,ps)->({ls & shift = False},ps))
	   		] (Columns 1) (if inishift 1 2)
	   		[ControlPos (Left,zero)
	   		]
	   	:+:	ButtonControl "Ok"
  			[ ControlId okId
  			, ControlFunction (okFun rgbid lsid dialogId) 
  			, ControlPos (Right, zero)
  			, ControlWidth (ContentWidth "Cancel")
  			]
    	:+:	ButtonControl "Cancel"
  			[ ControlPos (LeftOfPrev, zero) 
  			, ControlFunction (cancelFun inistate dialogId)
  			, ControlId cancelId
  			] 
		:+: ButtonControl "Apply"
			[ ControlPos (LeftOfPrev,zero)
			, ControlFunction (applyFun rgbid lsid)
			]
  		:+: ButtonControl "Paste"
  			[ ControlPos (LeftOfPrev,zero)
  			, ControlFunction (pasteFun rgbid hcid fcid bcid)
  			]
  		:+: ButtonControl "Copy"
  			[ ControlPos (LeftOfPrev,zero)
  			, ControlFunction (copyFun rgbid)
  			]

  		:+: Receiver lsid lsfun []
		)
	# dialog		=
		Dialog "Project Window..." controls 
  		[ WindowId dialogId 
  		, WindowOk okId
  		, WindowCancel cancelId
  		, WindowClose (cancelFun inistate dialogId)
  		, WindowInit (setBoxCol hcid fcid bcid)
  		]
	# (_,ps) = openModalDialog inistate dialog ps
	= ps
where
	toStringC :: !Colour -> String
	toStringC c = toString c
	copyFun rgbid (ls,ps)
		# ps = getColourBoxColour rgbid cont ps
		= (ls,ps)
	where
		cont col ps
			| isNothing col
				= ps
			# col = fromJust col
			# ps	= setClipboard [toClipboard (toString col)] ps
			= ps
	pasteFun rgbId hcId fcId bcId (ls=:{cr},ps)
		// get clipboard
		// and put in active colour control
		# (its,ps)	= getClipboard ps
		| isEmpty its
			= (ls,ps)
		# its		= map fromClipboard its
		# its		= filter isJust its
		| isEmpty its
			= (ls,ps)
		# it		= fromJust (hd its)
		| it <> toStringC (fromString it)
			= (ls,ps)
		# col		= fromString it
		# ls		= case cr of
						HC -> {ls & hc = col}
						FC -> {ls & fc = col}
						BC -> {ls & bc = col}
		# (ls,ps)	= setBoxCol hcId fcId bcId (ls,ps)
		# ps		= setColourBoxColour` rgbId col ps
		= (ls,ps)
	mfilter (MouseDown _ _ _) = True
	mfilter _ = False
	mfun rgbid lsid hcId fcId bcId ac _ (ls=:{fc,bc,hc,cr},ps)
		| cr == ac = (ls,ps)
		# ps = getColourBoxColour rgbid cont ps
		= (ls,ps)
	where
		cont col ps
			| isNothing col = ps
			# col = fromJust col
			# ps	= appPIO (case ac of
								HC	-> SetColourBox` hcId (case cr of
									HC	-> toRGBColour col
									_	-> toRGBColour hc
									)
								_	-> SetColourBox hcId (case cr of
									HC	-> toRGBColour col
									_	-> toRGBColour hc
									)
							) ps
			# ps	= appPIO (case ac of
								FC	-> SetColourBox` fcId (case cr of
									FC	-> toRGBColour col
									_	-> toRGBColour fc
									)
								_	-> SetColourBox fcId (case cr of
									FC	-> toRGBColour col
									_	-> toRGBColour fc
									)
							) ps
			# ps	= appPIO (case ac of
								BC	-> SetColourBox` bcId (case cr of
									BC	-> toRGBColour col
									_	-> toRGBColour bc
									)
								_	-> SetColourBox bcId (case cr of
									BC	-> toRGBColour col
									_	-> toRGBColour bc
									)
							) ps
			# cId = case ac of
						HC -> hcId
						FC -> fcId
						BC -> bcId
			# col` = case ac of
						HC -> hc
						FC -> fc
						BC -> bc
			# ps	= setColourBoxId rgbid cId ps
			# ps	= setColourBoxColour` rgbid col` ps
			# (_,ps) = asyncSend lsid (cont2 col) ps
			= ps
		cont2 col (ls=:{cr},ps)
			# ls	= case cr of
						HC -> {ls & hc = col, cr = ac}
						FC -> {ls & fc = col, cr = ac}
						BC -> {ls & bc = col, cr = ac}
			= (ls,ps)
	setBoxCol hcId fcId bcId (ls=:{cr},ps)
		# (cId,col) = case cr of
						HC	-> (hcId,ls.hc)
						FC	-> (fcId,ls.fc)
						BC	-> (bcId,ls.bc)
		# ps = appPIO (SetColourBox` cId (toRGBColour col)) ps
		= (ls,ps)
	applyFun rgbid lsid (ls,ps)
		= cset rgbid lsid id (ls,ps)
	okFun rgbid lsid dialogId (ls,ps)
		= cset rgbid lsid (noLS (closeWindow dialogId)) (ls,ps)
	cancelFun inils dialogId (ls,ps)
		# ls = inils
		# (ls,ps) = apply (ls,ps)
		= (ls,closeWindow dialogId ps)
	lsfun f (ls,ps)
		= f (ls,ps)
	cset rgbid lsid finish (ls=:{cr},ps)
		# ps = getColourBoxColour rgbid cont1 ps
		= (ls,ps)
	where
		cont1 col ps
			| isNothing col = ps
			# col = fromJust col
			# (_,ps) = asyncSend lsid (cont4 col) ps
			= ps
		cont4 col (ls,ps)
			# ls		= case cr of
							HC -> {ls & hc = col}
							FC -> {ls & fc = col}
							BC -> {ls & bc = col}
			# (ls,ps) = apply (ls,ps)
			= finish (ls,ps)
	apply (ls,ps)
		# (lbId,ps)	= getPWI ps
		# ((fnt_ok,fnt),ps)	= accScreenPicture (openFont {fName = ls.PWO.fn, fSize = ls.PWO.fs, fStyles = []}) ps
		# pen2		= if fnt_ok [PenFont fnt] []
		# pen		= [PenColour (ls.fc), PenBack (ls.bc):pen2] 

		# ps			= closeAllExtListBoxItems lbId ps

		# ps		= setExtListBoxPen lbId pen ps
		# (wId,ps)	= getPWW ps
		# ps		= appPIO (setWindowLook wId True (True,(\_ {newFrame} -> fill newFrame o setPenColour (ls.hc)))) ps
		# (prefs,ps) = getPrefs ps
		# prefs		=	{ prefs
						& prj_prefs.proj_forc = ls.fc
						, prj_prefs.proj_bacc = ls.bc
						, prj_prefs.proj_topc = ls.hc
						, prj_prefs.proj_font = {prefs.prj_prefs.proj_font & fName = ls.PWO.fn, fSize = ls.PWO.fs}
						, prj_prefs.proj_shft = ls.shift
						}
		# ps		= setPrefs prefs ps
		| not fnt_ok
			= (ls,ps)
		// set FlexBar pens...
		# (xId,ps)	= getPWX ps
		# (mId,ps)	= getPWM ps
		# ps = addFlexPens [(xId,pen2),(mId,pen2)] ps

		# ps = pm_update_project_window ps
		
		= (ls,ps)

    left = ControlPos (Left, zero)
	fontfun name (ls,ps)
		# ls		= {PWO | ls & fn = name}
		= (ls,ps)
	sizefun size (ls,ps)
		# ls		= {PWO | ls & fs = size}
		= (ls,ps)


//--

pm_open_project_window :: !*(PSt *General) -> *(PSt *General)
pm_open_project_window ps
	# (menuIds,ps)		= getMenuIds ps
	# (prefs,ps)		= getPrefs ps
	# pw_xvec			= prefs.prj_prefs.proj_pos
	# pw_xsiz			= prefs.prj_prefs.proj_siz
	# (wId,ps)			= getPWW ps
	# (lbId,ps)			= getPWI ps
	# (xxId,ps)			= getPWX ps
	# (mmId,ps)			= getPWM ps
	# (lcId,ps)			= openId ps
	# (lc2Id,ps)		= openId ps
	# (lc3Id,ps)		= openId ps
	# pw_main			= ""
	# pw_exec			= ""
	# (fnt,ps)			= accScreenPicture (getFont prefs.prj_prefs.proj_font) ps
	# pen				= [PenBack prefs.prj_prefs.proj_bacc, PenColour prefs.prj_prefs.proj_forc, PenFont fnt]
	# topc				= \butw -> top_controls lbId.controlId lcId lc2Id lc3Id xxId mmId pw_main pw_exec butw fnt
	# (pw_min_size,ps)	= controlSize (topc 0) True Nothing Nothing Nothing ps
	# lbox				= \ini siz -> PWListBox ini lbId lc2Id
							(PlatformDependant
							 (/* Win */ pw_min_size.Size.h+4)
							 (/* Mac */ pw_min_size.Size.h)
							 )siz pen
	# pw_xpos			= (LeftTop,OffsetVector pw_xvec)
	# butwidth			= pw_xsiz.w
	# (err,ps)			= openWindow Void (
							ProjectWindow "project"
							(lbox False {Size | pw_xsiz & h = pw_xsiz.Size.h - 
								PlatformDependant
								(/* Win */ pw_min_size.Size.h+4)
								(/* Mac */ pw_min_size.Size.h)
								})
							lbId.controlId
							(topc butwidth)
							wId
							pw_xsiz
							pw_xpos
							prefs.prj_prefs.proj_topc
							menuIds
							) ps
	| err <> NoError = abort "unable to open Project window"
	= ps
where
	proj_activate ps
		# ({mn_clo,mn_sav,mn_sva,mn_und,md_cmp,md_chk,md_gen,md_cst,md_est,mn_cut,mn_cpy,mn_pst,mn_clr,mg_edt,searchIds,moduleIds},ps=:{io})
			= getMenuIds ps
		# io = setMenuElementTitles
				[(mn_sav,"Save Project")
				,(mn_und,"&Undo")						// shouldn't be here...
				] io
		# l1 = [mn_und,mn_clo,mn_cut,mn_cpy,mn_pst,mn_clr:mg_edt]
		# io = disableMenuElements l1 io
		# io = disableMenuElements searchIds.findIds io
		# io = disableMenuElements searchIds.gotoIds io
		# projModuleIds = removeMembers moduleIds [md_cmp,md_chk,md_gen]
		# io = enableMenuElements projModuleIds io
		# l2 = removeMembers [mn_sav,mn_sva,md_cmp,md_chk,md_gen,md_cst,md_est] projModuleIds
		# io = disableMenuElements l2 io
		= {ps & io = io}

	proj_deactivate ps
		# ({mn_clo,mn_sav,mn_sva,md_cmp,md_chk,md_gen,md_cst,md_est,mn_cut,mn_cpy,mn_pst,mn_clr,mg_edt,searchIds},ps=:{io})
			= getMenuIds ps
		# io = setMenuElementTitles [(mn_sav,"Save")] io
		# io = disableMenuElements [mn_sav,mn_sva,md_cmp,md_chk,md_gen,md_cst,md_est] io
		# io = enableMenuElements [mn_clo,mn_cut,mn_cpy,mn_pst,mn_clr:mg_edt] io
		# io = enableMenuElements searchIds.findIds io
		# io = enableMenuElements searchIds.gotoIds io
		= {ps & io = io}

	getFont fd pic
		# ((ok,fnt),pic) = openFont fd pic
		| not ok
			= openDialogFont pic
		= (fnt,pic)

	ProjectWindow title lb lbId tc wId pw_xsiz pw_xpos topc menuIds
		= Window title
			(	tc
			:+:	lb
			)
			[ WindowId			wId
			, WindowViewSize	pw_xsiz
			, WindowPos			pw_xpos
			, WindowPen			[PenBack topc]
			, WindowLook		True (\_ {newFrame,updArea} -> seq (map unfill updArea))
			, WindowInit		(noLS (proj_activate o SetWindowIcon wId ProjectIcon))
			// setWindowViewSize forces resize of listbox... actually forces proper init...
			, WindowInitActive	lbId
			, WindowActivate	(noLS proj_activate)
			, WindowDeactivate	(noLS proj_deactivate)
			]

	PWListBox ini lbId lc2Id minh size pen
		= ExtListBoxControl
						[]	// contents
						[]	// initial selection
						(\_ ps -> ps)
						lbId
						[ ControlOuterSize size
						, ControlResize (\_ _ nw -> {Size | nw & h = nw.Size.h - minh})
						, ControlPos (if ini (Left,zero) (Below lc2Id,OffsetVector {zero & vx = ~10, vy = 10}))
						, ControlPen pen
						, ControlHMargin 10 10
						, ControlVMargin 3 3
						, ControlMouse mouseFilter Able mouseFunction
						]

	top_controls lbId localId local2Id local3Id xxId mmId pw_main pw_exec butw fnt =
			(
				FlexText pw_exec xxId
					[ ControlPen [PenColour Black,PenBack LightGrey, PenFont fnt]
					, ControlResize (\cc _ nw -> {cc & w = nw.w - 20})
					, ControlId local3Id
					, ControlPos (Fix,OffsetVector {vx=10,vy=10})
					, ControlWidth (PixelWidth (butw-20))
					]
			:+:	FlexText pw_main mmId
					[ ControlPen [PenColour Black,PenBack LightGrey, PenFont fnt]
					, ControlResize (\cc _ nw -> {cc & w = nw.w - 20})
					, ControlPos (Below local3Id,OffsetVector {zero & vy = 5})
					, ControlId local2Id
					, ControlWidth (PixelWidth (butw-20))
					]
			)
	
pm_update_project_window :: !*(PSt *General) -> *PSt *General
pm_update_project_window ps
	# (lbId,ps)		= getPWI ps
	# (xxId,ps)		= getPWX ps
	# (mmId,ps)		= getPWM ps
	# (prefs,ps)	= getPrefs ps

	# (modules,ps)	= accProject (\l->(PR_GetModuleStuff l,l)) ps
	# (prjpaths,ps)	= accProject (\l->(PR_GetPaths l,l)) ps
	# (xp,ps)		= accProject (\l->(PR_GetExecPath l,l)) ps
	# (mm,ps)		= accProject (\l->(PR_GetRootModuleName l,l)) ps
	# (wId,ps)		= getPWW ps
	# (ws,ps)		= accPIO (getWindow wId) ps
	# (ok,mframe)	= case ws of
						Nothing		-> (False,Nothing)
						Just ws		-> getControlViewFrame lbId.controlId ws
	# ps			= closeAllExtListBoxItems lbId ps
	# (syspaths,ps)	= getCurrentPaths ps
	# srcpaths		= AppendLists prjpaths syspaths
	# (appPath,ps)	= getStup ps
	# (prjPath,ps)	= getPath ps
	# prjPath		= RemoveFilename prjPath
	# lbItems		= items srcpaths appPath prjPath modules prefs.prj_prefs.proj_shft
	# ps			= appendExtListBoxItems lbId lbItems ps
	# ps			= case mframe of
						Nothing			-> ps
						(Just jframe)	# ps	= appPIO (moveControlViewFrame lbId.controlId (toVector jframe.corner1)) ps
//										# ps	= updateControl	// useless for now since moveviewframe & setviewdomain both already trigger updates...
										-> ps
	# xp			= symPath appPath prjPath xp
	# mm			= symPath appPath prjPath mm
	# ps			= setFlexTexts [(xxId,xp),(mmId,mm)] ps
	= ps
where
	items srcpaths appPath prjPath modules shift
		# mods			= StrictListToList modules
		| isEmpty mods
			= []
		# [(root,_,_,_):mods]	= mods
		# mods					= sortBy (\(a,b,_,_) (c,d,_,_) -> less a b c d) mods
		# moditems				= makenice "" mods
		# rootitem				= (GetModuleName root, OpenModule (MakeImpPathname root) emptySelection, OpenModule (MakeDefPathname root) emptySelection)
		= [rootitem:moditems]
	where
		less a b c d
			| before b d = True		// use < -ordening of searchpaths...
			| b == d
				= a < c
			= False
		before x y = bf srcpaths
		where
			bf Nil = False
			bf (p:!r)
				| p == y = False
				| p == x = True
				= bf r
		makenice _ [] = []
		makenice s l=:[(a,b,_,_):r]
			| s <> b	// new directory
				= [pw_separator b: makenice b l]
			= [(GetModuleName a,f a, f` a):makenice s r]	// add seperators...
		where
			f mod	= if shift
						(OpenModule (MakeDefPathname mod) emptySelection)
						(OpenModule (MakeImpPathname mod) emptySelection)
			f` mod	= if shift
						(OpenModule (MakeImpPathname mod) emptySelection)
						(OpenModule (MakeDefPathname mod) emptySelection)
			pw_separator dir
				# dir = symPath appPath prjPath dir
				= ("\\\\--- "+++dir,id,id)					// build fold/unfold functionality here?!
	
// pm_set: set main module
pm_set :: !*(PSt *General) -> *PSt *General
pm_set ps
	# (win,ps)	= accPIO getActiveWindow ps
	| isNothing win
		= okNotice ["Unable to set main module.","There is no active module window."] ps
	# win		= fromJust win
	# (path,ps)	= message win msgGetPathName ps
	| isNothing path
		= okNotice ["Unable to set main module.","There is no active module window."] ps
	# path			= fromJust path
	# (project,ps)	= accProject (\l->(l,l)) ps
	// check if mod in project already has settings...
	# info			= PR_GetModuleInfo (GetModuleName path) project
	| isJust info
		# info	= fromJust info
		# eo	= info.impeo
		# co	= info.compilerOptions
		# project = PR_SetRoot path eo co project
		# ps	= appProject (const project) ps
		# ps	= pm_update_project_window ps
		= ps
	# (pos,ps)	= accPIO (getWindowPos win) ps
	| isNothing pos
		= ps	// this should not occur
	# pos		= fromJust pos
	# (siz,ps)	= accPIO (getWindowViewSize win) ps
//	# (fnt,ps)	= sendToActiveWindow msgGetFont ps
//	| isNothing fnt
//		= ps	// this should not occur
//	# fnt		= fromJust fnt
//	# fnt		= getFontDef fnt
//	# (tbs,ps)	= sendToActiveWindow msgGetTabs ps
//	| isNothing tbs
//		= ps	// this should not occur
//	# tbs		= fromJust tbs
//	# (lns,ps)	= sendToActiveWindow getLineNumbers ps
//	| isNothing lns
//		= ps	// this should not occur
//	# lns		= fromJust lns
	# (nlc,ps)	= sendToActiveWindow getNewlineConvention ps
	| isNothing nlc
		= ps	// this should not occur
	# nlc		= fromJust nlc
//	# (sci,ps)	= sendToActiveWindow getFontInfo ps
//	| isNothing sci
//		= ps	// this should not occur
//	# {showSyntax}		= fromJust sci
	
	# pos_size	= {posx = pos.vx,posy=pos.vy,sizex=siz.w,sizey=siz.Size.h}
	# eo		= {newlines = nlc}
	# weo		= {eo = eo,pos_size = pos_size}
	#	wco				= DefaultCompilerOptions
		project`		= PR_SetRoot path weo wco project
		ps				= appProject (const project`) ps
		ps = pm_update_project_window ps
	= ps
		
pm_compile :: !*(PSt *General) -> *PSt *General;
pm_compile ps
	= DoProcess "compiling"  (CompileProjectModule Compilation) (\_ _ _ _-> id) ps
	
pm_check :: !*(PSt *General) -> *PSt *General;
pm_check ps
	= DoProcess "checking"  (CompileProjectModule SyntaxCheck) (\_ _ _ _-> id) ps

pm_gen_asm :: !*(PSt *General) -> *PSt *General;
pm_gen_asm ps
	= DoProcess "generating"  GenAsmProjectModule open_gen_asm ps
where
	open_gen_asm winpath modname ok newpaths ps
		| not ok
			= ps
		# apath			= MakeAssemblySystemPathname winpath
		= ed_open_assembly apath ps
	ed_open_assembly pathName ps
		# (mFHrecId,ps)	= getFHI ps
		# (_,ps)		= syncSend2 mFHrecId pathName ps
		= ed_open_cont pathName cont ps
	where
		cont wasopen win ps
			| wasopen
				= ed_revert_win win ps
			# (_,ps) = message win mess ps
			= ps
		mess es
			# ((_,b,c),es) = msgGetTabs es
			= msgSetTabs (8,b,c) es
			

pm_upto :: !Bool !*(PSt *General) -> *PSt *General;
pm_upto force ps
	# ps = ed_ask_save_all False (BringProjectUptoDate force cont) ps
	= mb_update_undoinfo ps
where
	cont path linked ok ps
		| linked || not ok
			= closeInfo ps
		= showInfo (Level1 "Project is up to date") ps

pm_exec :: !*(PSt *General) -> *PSt *General;
pm_exec ps
	# ps = ed_ask_save_all False (BringProjectUptoDate False cont) ps
	= mb_update_undoinfo ps
where
	cont execpath linked ok ps
		# ps		= closeInfo ps
		| not ok
			= ps
		# (lo,ps)	= accProject (\project->(PR_GetLinkOptions project,project)) ps
		# (prj_path,ps)				= getPath ps
		# (app_path,ps)				= getStup ps
		| lo.method == LM_Dynamic
			# (dynlstr,ps)				= getCurrentDynl ps
			# dyn_start					= (app_path +++ {dirseparator} +++dynlstr)
			# (ok,ps)					= DynLink dyn_start prj_path app_path ps
			| not ok
				= okNotice ["Unable to start dynamic linker:",dyn_start] ps
			= ps
		# prj_path`					= RemoveFilename prj_path
		# execpath					= fulPath app_path prj_path` execpath
		= RunProgram execpath ps

pm_run :: !*(PSt *General) -> *PSt *General;
pm_run ps
	# (app_path,ps)				= getStup ps
	# (prj_path,ps)				= getPath ps
	# (execpath,ps)				= accProject (\l->(PR_GetExecPath l,l)) ps
	# prj_path`					= RemoveFilename prj_path
	# execpath					= fulPath app_path prj_path` execpath
	# (lo,ps)					= accProject (\project->(PR_GetLinkOptions project,project)) ps
	| lo.method == LM_Dynamic
		# (dynlstr,ps)				= getCurrentDynl ps
		# dyn_start					= (app_path +++ {dirseparator} +++dynlstr)
		# (ok,ps)					= DynLink dyn_start prj_path app_path ps
		| not ok
			= okNotice ["Unable to start dynamic linker:",dyn_start] ps
		= ps
	= RunProgram execpath ps

//--

pm_build :: !.a -> .a;
pm_build ps			// rebuild project from scratch
	= ps

pm_link :: !.a -> .a;
pm_link ps			// relink project
	// fake it by deleting exe
	= ps

pm_copt :: !*(PSt *General) -> *PSt *General
pm_copt ps
	//-> only for module + ide, now added cursel in projwin
	// current active added... (need to check if active is part of project...)
	# (lbId,ps)			= getPWI ps
	# (project,ps)		= accProject (\l->(l,l)) ps
	# (path,ps)			= sendToActiveWindow msgGetPathName ps
	| isJust path
		# path			= fromJust path
		# mod			= GetModuleName path
		# minf			= PR_GetModuleInfo mod project
		| isNothing minf
			// module not found in project...
			// currently do nothing (could add to proj, but disappears when not in deps...)
			= okNotice ["This module is not part of the current project."] ps
		# minf			= fromJust minf
		# projco		= minf.compilerOptions
		# setco			= \ao -> appProject (\l->PR_UpdateModule mod (\mi->{mi & compilerOptions = ao}) l)
		= doCompilerOptionsDialog "Module Options" projco setco ps
	// should check if project window is active...
	// otherwise this behaviour is unintuitive
	# (pwId,ps)			= getPWW ps
	# (awId,ps)			= accPIO getActiveWindow ps
	| isNothing awId || pwId <> fromJust awId
		= ps
	# ((hassel,sel),ps)	= getExtListBoxSelection lbId ps
	| hassel && notEmpty sel
		// work on sel ...
		# sel			= map fst sel
		# mod			= hd sel
		# minf			= PR_GetModuleInfo mod project
		| isNothing minf
			= ps
		# minf			= fromJust minf
		# projco		= minf.compilerOptions
		# setco			= \ao -> appProject (\l->PR_UpdateModules sel (\mi->{mi & compilerOptions = ao}) l)
		= doCompilerOptionsDialog "Module Options" projco setco ps
	// work op nothing... ?!
	= ps

pm_coprefs :: !*(PSt *General) -> *PSt *General;
pm_coprefs ps
	# (prefs=:{compopts},ps) = getPrefs ps
	# setco = \co -> setPrefs {prefs & compopts = co}
	= doCompilerOptionsDialog "Module Defaults" compopts setco ps

DoProcess msg compile cont ps
	#	(winpath,ps)		= sendToActiveWindow msgGetPathName ps
	| isNothing winpath
		= ps
	#	winpath 			= fromJust winpath
	| not (IsImpPathname winpath)
		= ps
	#	modname				= GetModuleName winpath
	#	ps					= ed_ask_save_all False (okcont winpath modname) ps
	#	ps					= mb_update_undoinfo ps
	= ps
where
	okcont winpath modname ps	
		#	(project,ps)		= accProject (\l->(l,l)) ps
			ps					= ew_safe_close ps
			ps					= tw_safe_close ps
		= compile winpath project (setproject winpath modname) ps
	
	setproject winpath modname ok newpaths newproject ps
		#	ps = closeInfo ps
		# ps = appProject (const newproject) ps
		# ps = pm_save ps
		# ps = pm_update_project_window ps
		= cont winpath modname ok newpaths ps

pm_save_as :: !*(PSt *General) -> *PSt *General
pm_save_as ps
	# (pn,ps)	= selectOutputFile "Save Project as:" "*.prj" ps
	| isNothing pn
		= ps
	# pn		= fromJust pn
	# ps		= setPath pn ps
	# ps		= pm_save ps
	# (recId,ps) = getPHI ps
	# (_,ps)	= syncSend2 recId pn ps
	# ps		= pm_set_window_title pn ps
	= pm_menu_add pn ps

pm_save_copy_as :: !*(PSt *General) -> *PSt *General
pm_save_copy_as ps
	# (pn,ps)	= selectOutputFile "Save copy of Project as:" "*.prj" ps
	| isNothing pn
		= ps
	# pn		= fromJust pn
	#	(project,ps)	= accProject (\l->(l,l)) ps
	# (project,ps)		= pm_save_common pn project ps
	#	ps				= appProject (const project) ps
	= ps

pm_save :: !*(PSt *General) -> *PSt *General
pm_save ps
	#	(project,ps)	= accProject (\l->(l,l)) ps
		(pathname,ps)	= getPath ps
	# (project,ps) = pm_save_common pathname project ps
	#	project			= PR_SetSaved project
		ps				= appProject (const project) ps
	= ps

pm_save_common pathname project ps
	| pathname == ""
		= (project,ps)	// don't save dummy project...

	#	(startupdir,ps)	= getStup ps
		prjpath			= MakeProjectPathname pathname
		(ok,ps)			= accFiles (SaveProjectFile prjpath project startupdir) ps
	| not ok
		#	ps			= okNotice
							[ "The file "+++RemovePath prjpath+++" could not be saved because"
							, "of a file I/O error."
							] ps
		= (project,ps)
	# (prefs,ps)		= getPrefs ps
	| not prefs.enable_prover
		= (project,ps)
	# (syspaths,ps)		= getCurrentPaths ps
	# prjpaths			= PR_GetPaths project
	# srcpaths			= AppendLists prjpaths syspaths
	# stuff				= PR_GetModuleStuff project
	# dclstuff			= StrictListToList (Map (\(m,d,_,_)->(GetModuleName m,d)) stuff)
	# iclstuff			= StrictListToList (Map (\(_,_,m,d)->(GetModuleName m,d)) stuff)
	# [(rootname,rootpath) : iclstuff]
						= iclstuff
	# proveropts		=
	   { project_name         = RemoveSuffix (RemovePath prjpath)
	   , project_paths        = StrictListToList srcpaths
	   , main_module_name     = rootname
	   , main_module_path     = rootpath
	   , icl_modules          = iclstuff
	   , dcl_modules          = dclstuff
	   }
	# (ok,ps)			= accFiles (WriteProverOptions (make_prover_name pathname) proveropts) ps
	| not ok
		#	ps			= okNotice
							[ "The prover file for "+++RemovePath prjpath+++" could not be saved because"
							, "of a file I/O error."
							] ps
		= (project,ps)
	= (project,ps)

pm_maybe_save :: !Id !*(PSt *General) -> (Bool,*PSt *General)
pm_maybe_save win ps
	// if win is project window -> save project
	# (wId,ps) = getPWW ps
	| wId == win
		= (True,pm_save ps)
	= (False,ps)

pm_set_window_title :: .Title !*(PSt *General) -> *PSt *General;
pm_set_window_title pathname ps
	# (wId,ps) = getPWW ps
	# name = RemovePath pathname
	= appPIO (setWindowTitle wId (name+++" - "+++pathname)) ps


make_prover_name name
	# name = MakeProjectPathname name
	# name = RemoveSuffix name
	# name = name +++ ".pr_"
	= name

pm_get_projwin_possiz :: *(PSt *General) -> *(.(Vector2,Size),*PSt *General);
pm_get_projwin_possiz ps
	# (wId,ps)	= getPWW ps
	# (siz,ps)	= accPIO (getWindowViewSize wId) ps
	# (vec,ps)	= accPIO (getWindowPos wId) ps
	# vec		= fromJust vec
	= ((vec,siz),ps)

isProjWin :: !Id !*(PSt *General) -> (!Bool,!*PSt *General)
isProjWin win ps
	# (wId,ps) = getPWW ps
	= (wId == win,ps)
