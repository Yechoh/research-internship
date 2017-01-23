implementation module IDE

//*********************************************************************************
// Original Clean Library Software Module
// Written for Clean version  : 1.3
// Written for I/O version    : 1.2
// Author                     : Diederik van Arkel
// Date                       :
// Last Modified by           :
// Date                       :
// Copyright                  : 1999 Hilt - High Level Software Tools B.V.
//                            : University of Nijmegen
// e-mail                     : clean@cs.kun.nl or rinus@hilt.nl
//*********************************************************************************
// It is allowed to modify this module for your own purposes but it is NOT allowed
// to (re)distribute the code or the modified code in ANY form without written
// permission.
//*********************************************************************************


//=>*********************************************************************************
//=> Clean Integrated Development Environment Module
//=> Copyright                  : 1999 -- 2001
//=>							: Hilt - High Level Software Tools B.V.
//=>                            : University of Nijmegen
//=> e-mail                     : clean@cs.kun.nl or rinus@hilt.nl
//=> License					: xxxxx
//=>*********************************************************************************

import StdArray, StdEnum, StdFunc, StdMisc, StdTuple
import StdFileSelect,StdMenu,StdMenuReceiver,StdProcess, StdPStClass
import ExtNotice
import UtilIO

import PmDialogues
import PmParse
import PmPath

from EdKeyMapping import macKeyMapping, ReadKeyMapFile
from finder import sr_find, sr_find_next, sr_find_sel, sr_rep_find, sr_goto_cursor
from finder import sr_goto_line, sr_goto_selection
from edoptions import defaultFontAndTabs, optionsKeyMapping, editColours

from EdClient import EditAction, isEditWin, msgGetPathName,msgSelectAll
from EdClient import emptySelection, sendToActiveWindow, msgGetSelection
from EdClient import msgUndo,msgPaste,msgCopy,msgCut,msgClear,msgBalance,msgDetab
import prefix, tools

import IdeState
import ideoptions

import clipboard,typewin,idehelp,PmEnvironment,search
import errwin, messwin, projwin, edfiles
import projmen, filehist, ioutil, menubar
from PmDirCache import SearchDisk
import targetui
from PmCleanSystem import QuitCleanCompiler
import Platform, IdePlatform

trace_n _ f :== f

//--

Start :: !*World -> *World
Start world
	#	(stup,world)		= accFiles GetFullApplicationPath world
		(mEditId,world) 	= openId world
		(mEdUndoId,world)	= openId world
		(mMdEdOptId,world)	= openId world

	# prefspath				= MakeFullPathname PrefsDir	PrefsFileName//applicationpath PrefsFileName
	#!	(prefs,world)		= openPrefs prefspath world

	# envspath				= MakeFullPathname EnvsDir EnvsFileName	//applicationpath EnvsFileName
	#!	(iniTargets,world)
							= openEnvironments stup envspath world

	#	(ffind,world)		= initFindBoxInfo prefs world
	#	(iniTwi,world)		= iniTypeWinInfo mEditId mEdUndoId ([]) prefs.typ_prefs world
// Win only?
	#	(iniCons,world)		= iniConsWinInfo mEditId mEdUndoId ([]) prefs.con_prefs world
	#!	(lbId,world)		= openExtListBoxId world
	#!	(mTargetId,world)	= openId world
		(eTargetId,world)	= openId world
		(mProjectId,world)	= openId world
		(mPrListId,world)	= openId world
	#!	(mPrRecId,world)	= openR2Id world
	#	(toolbar,world)		= setupToolBar prefs.show_toolbar world
	#	(keymap,world)		= setupKeyMap world
	#	emptyFindInfo		= { fi_find = [], fi_repl = [], fi_ic = False, fi_wa = True
							  , fi_bw = False, fi_mw = False, fi_re=False
							  }
	#!	(iniClip,pub,world)	= iniGeneral
								prefs
								stup
								mTargetId
								eTargetId
								lbId
								(initEditorState keymap)
								emptyFindInfo
								ffind
								mEdUndoId
								mMdEdOptId
								iniTwi
// winOnly?
								iniCons
								iniTargets
								mProjectId
								mPrListId
								mPrRecId
								world
	#	patt				=	[ ProcessClose (Quit prefspath)
								, ProcessOpenFiles openfiles
//								, ProcessClipboardChanged clip_changed
								: toolbar
								] ++ PlatformProcessAttributes
	#!	(ids,world)			= openIds 8 world
	#	(mFhMenId,world)	= openId world
		(mPhMenId,world)	= openId world
		pini				= ini envspath prefspath
								mEditId mEdUndoId iniClip iniTargets mTargetId
								eTargetId mProjectId mPrListId mPrRecId mFhMenId
								mPhMenId ids prefs
	= startIO MDI pub pini patt world
where
	setupKeyMap world
		# ((km,ok,_),world) = accFiles (ReadKeyMapFile (applicationpath "default.km")) world
		# keymap			= if ok km macKeyMapping
		= (keymap,world)
	setupToolBar show world		// need to conditionalise for Mac...
		| not show
			= ([],world)
		# items					= []
		# (items,world)			= toolIconFun
									"bitmaps\\srchBM.bmp"
									(Just "Search...")
									(sr_find_idi True)
									items world
		# (items,world)			= toolIconFun
									"bitmaps\\findBM.bmp"
									(Just "Find...")
									sr_find
									items world
		# items					= if (isEmpty items) [] [ToolbarSeparator:items]
		# (items,world)			= toolIconFun
									"bitmaps\\execBM.bmp"
									(Just "Run")
									pm_run
									items world
		# (items,world)			= toolIconFun
									"bitmaps\\updtBM.bmp"
									(Just "Update")
									(pm_upto False)
									items world
		# (items,world)			= toolIconFun
									"bitmaps\\urunBM.bmp"
									(Just "Update and Run")
									pm_exec
									items world
		# items					= if (isEmpty items) [] [ToolbarSeparator:items]
		# (items,world)			= toolIconFun
									"bitmaps\\prntBM.bmp"
									(Just "Print...")
									ed_print
									items world
		# items					= if (isEmpty items) [] [ToolbarSeparator:items]
		# (items,world)			= toolIconFun
									"bitmaps\\saveBM.bmp"
									(Just "Save")
									(ide_save NoModifiers)
									items world
		# (items,world)			= toolIconFun
									"bitmaps\\openBM.bmp"
									(Just "Open...")
									(ed_open NoModifiers)
									items world
		# (items,world)			= toolIconFun
									"bitmaps\\newfBM.bmp"
									(Just "New...")
									(ed_new "*.icl")
									items world
		| isEmpty items
			= ([],world)
		# toolbar			=	[ ProcessToolbar items]
		= (toolbar,world)
	toolIconFun toolname tooltip toolfun itemlist world
		# (bmp,world)	= openBitmap (applicationpath toolname) world
		# itemlist		= case bmp of
							Nothing		-> itemlist
							Just bmp	-> [ToolbarItem bmp tooltip toolfun:itemlist]
		= (itemlist,world)
	ini envspath prefspath mEditId mEdUndoId iniClip iniTargets mTargetId eTargetId
		mProjectId mPrListId mPrRecId mFhMenId mPhMenId
		[ mFileId
		, mSearchId
		, mCommandsId
		, mOptionsId, mGoodiesId
		, mPrNewId, mPrOpenId
		, quitId
		: _
		] prefs=:{altgr_workaround} ps

		# (fhRecId,ps)	= getFHI ps
		# (phRecId,ps)	= getPHI ps
		# (menuIds,ps)	= getMenuIds ps
		# (err,ps)		= openMenu Void (fileMenu prefspath menuIds mPrNewId mFileId mFhMenId mPhMenId quitId fhRecId phRecId prefs) ps
		| err <> NoError
			= abort "unable to open File menu"
		# (editRecId,ps)= getEditRecId ps
		# (err,ps)		= openMenu iniEditLS (editMenu altgr_workaround mEditId editRecId menuIds.mn_sav menuIds.mn_rev menuIds iniClip) ps
		| err <> NoError
			= abort "unable to open Edit menu"
		# (err,ps)		= openMenu Void (searchMenu altgr_workaround mSearchId menuIds.searchIds) ps
		| err <> NoError
			= abort "unable to open Search menu"
		# (err,ps)		= openMenu Void (projectMenu mProjectId mPrOpenId mPrListId mPrRecId menuIds prefs) ps
		| err <> NoError
			= abort "unable to open Project menu"
		# (err,ps)		= openMenu Void (moduleMenu menuIds) ps
		| err <> NoError
			= abort "unable to open Module menu"
		# (err,ps)		= openMenu Void (optionsMenu mOptionsId) ps
		| err <> NoError
			= abort "unable to open Defaults menu"
		# (err,ps)		= openMenu Void (targetMenu envspath iniTargets mTargetId eTargetId getTargets setTargets) ps
		| err <> NoError
			= abort "unable to open Environment menu"
		# ps			= setProjectTarget (hd iniTargets).target_name ps
		# ps			= pm_open_project_window ps
		# (wId,ps)		= getPWW ps
		# ps			= initHelpMenu wId ps
		# (twi,ps)		= accPLoc getTypeWinInfo ps
		# twi			= TW_SetUpd ([menuIds.mn_cut,menuIds.mn_pst,menuIds.mn_clr:menuIds.mg_edt]) twi
		# ps			= appPLoc (setTypeWinInfo twi) ps
		# (files,ps)	= initPlatformCommandLine ps
		# ps			= openfiles files ps
		# ps = SetProcessIcon CleanIcon ps
		= installPlatformEventHandlers ps
	ini _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ps = abort "IDE.icl: ini called with insufficient id's"

fileMenu prefspath {mn_clo,mn_sva,mn_sav,mn_rev,mn_oth,mn_prt,mn_prs,mn_odm,mn_oim}
	mPrNewId mFileId mFhMenId mPhMenId quitId fhRecId phRecId prefs

	# altgr_workaround	= prefs.altgr_workaround
	= Menu "&File"
		(	MenuItem	"&New File..."
				[ MenuShortKey 'N'
				, MenuModsFunction newffun
				]
		:+:	MenuItem "New Project..."
				[ MenuFunction (noLS pm_new)
				, MenuId mPrNewId
				]
		:+: MenuItem	"&Open..."
				[ MenuShortKey 'O'
				, MenuModsFunction (noLS1 ed_open)
				]
		:+:	MenuItem	"&Save"
				[ MenuShortKey 'S'
				, MenuModsFunction (noLS1 ide_save)
				, MenuId mn_sav
				]
		:+:	MenuItem	"Save As..."
				[ MenuModsFunction (noLS1 ide_save_as)
				, MenuId mn_sva
				]
		:+:	MenuItem	"Save All"
				[ MenuFunction (noLS ed_save_all)
				]
		:+:	MenuSeparator []
		:+:	MenuItem	"&Revert"
				[ MenuFunction (noLS ed_revert)
				, MenuId mn_rev
				, MenuSelectState Unable
				]
		:+: MenuItem	"&Close"
				[ MenuShortKey 'W'
				, MenuId mn_clo
				, MenuSelectState Able
				, MenuModsFunction (noLS1 ide_menu_close)
				]
		:+: MenuItem	"Close All"
				[ MenuFunction (noLS ide_close_all)
				]
		:+:	MenuSeparator []
		:+: MenuItem	"Open Other"
				[ MenuFunction (noLS ed_open_other)
				, MenuId mn_oth
				, MenuSelectState Unable
				: if altgr_workaround [] [MenuShortKey '/']
				]
		:+: MenuItem	"Open Definition..."
				[ MenuShortKey 'D'
				, MenuModsFunction (noLS1 (ed_open_sel False))
				, MenuId mn_odm
				]
		:+: MenuItem	"Open Implementation..."
				[ MenuShortKey 'I'
				, MenuModsFunction (noLS1 (ed_open_sel True))
				, MenuId mn_oim
				]
		:+:	MenuSeparator []
		:+: MenuItem	"Print Setup..."
				[ MenuId mn_prs
				, MenuFunction (noLS ed_print_setup)
				]
		:+:	MenuItem	"&Print..."
				[ MenuShortKey 'P'
				, MenuFunction (noLS ed_print)
				, MenuId mn_prt
				, MenuSelectState Unable
				]
		:+:	MenuItem	"Print All..."
				[ MenuFunction (noLS ed_print_all)
				]
		:+:	MenuSeparator []
		:+: FileHistMenu "Recent Files" 12 mFileId fhRecId mFhMenId (StrictListToList prefs.file_hist) (\s->ed_open_cont s (\_ _ -> id))
		:+: FileHistMenu "Recent Projects" 12 mFileId phRecId mPhMenId (StrictListToList prefs.proj_hist) pmopen
		:+:	MenuSeparator []
		:+:	MenuItem	"&Quit"
				[ MenuShortKey 'Q'
				, MenuFunction (noLS (Quit prefspath))
				, MenuId (quitId)
				]
		)
		[ MenuId (mFileId)
		]
where
	newffun mods (ls,ps)
		| not mods.shiftDown
			# ps	= case mods.altDown || mods.optionDown of
						True	-> ed_new "*.dcl" ps
						False	-> ed_new "*.icl" ps
			= (ls,ps)
		# ps		= pm_new ps
		= (ls,ps)
	pmopen pathName ps
		# (ok,ps)		= pm_switch pathName ps
		= ps
	
searchMenu altgr_workaround mSearchId {srchIds,findIds,gotoIds,nextIds}
	= Menu "&Search"
		(	MenuItem "&Find..."					[ MenuId (findIds!!0), MenuShortKey 'F', MenuModsFunction (shiftfun sr_find) ]
		:+:	MenuItem "Find &Next"				[ MenuId (findIds!!1), MenuShortKey 'G', MenuModsFunction (shiftfun sr_find_next) ]
		:+:	MenuItem "Find Selection"			[ MenuId (findIds!!2), MenuShortKey 'H', MenuModsFunction (shiftfun sr_find_sel) ]
		:+:	MenuItem "Replace && Find Again"	[ MenuId (findIds!!3), MenuShortKey 'T', MenuModsFunction (shiftfun sr_rep_find) ]
		:+: MenuSeparator						[]
		:+:	MenuItem "&Search Identifier"
			[ MenuId (srchIds!!0)
			, MenuModsFunction (shiftfun` sr_find_idi) 
			: if altgr_workaround [] [MenuShortKey '=']
			]
		:+:	MenuItem "Search Definition"		[ MenuId (srchIds!!1), MenuShortKey 'L', MenuModsFunction (shiftfun` sr_find_def) ]
		:+:	MenuItem "Search Implementation"	[ MenuId (srchIds!!2), MenuShortKey 'M', MenuModsFunction (shiftfun` sr_find_imp) ]
		:+: MenuSeparator						[]
		:+: MenuItem "Goto Next Search &Result"
			[ MenuId (nextIds!!0)
			, MenuSelectState Unable
			, MenuModsFunction findresfun
			: if altgr_workaround [] [MenuShortKey ',']
			]
		:+:	MenuItem "Goto Next &Error"			[ MenuId (nextIds!!1), MenuShortKey 'E', MenuSelectState Unable, MenuModsFunction finderrfun ]
		:+:	MenuItem "Goto &Cursor"				[ MenuId (gotoIds!!0), MenuFunction (noLS sr_goto_cursor) ]
		:+:	MenuItem "Goto &Line..."			[ MenuId (gotoIds!!1), MenuFunction (noLS sr_goto_line) ]
		:+:	MenuItem "Goto Selection..."		[ MenuId (gotoIds!!2), MenuFunction (noLS sr_goto_selection) ]
		)
		[ MenuId (mSearchId)
		]
where
	shiftfun fun mods (ls,ps)
		| not mods.shiftDown
			# ps	= fun ps
			= (ls,ps)
		# (fi,ps)	= getFI ps
		# fi		= {fi & fi_bw = not fi.fi_bw}
		# ps		= setFI fi ps
		# ps		= fun ps
		# (fi,ps)	= getFI ps	// so that we do get the modifications in find info from find selection...
		# fi		= {fi & fi_bw = not fi.fi_bw}
		# ps		= setFI fi ps
		= (ls,ps)
	shiftfun` fun mods (ls,ps)
		| not mods.shiftDown
			# ps	= fun False ps
			= (ls,ps)
		# ps		= fun True ps
		= (ls,ps)
	finderrfun mods (ls,ps)
		# ps = sr_find_err (not mods.shiftDown) ps
		= (ls,ps)
	findresfun mods (ls,ps)
		# ps = wind_next (not mods.shiftDown) ps
		= (ls,ps)
			
projectMenu
	mProjectId mPrOpenId
	mPrListId mPrRecId
	{projIds} prefs
	= Menu "&Project"
	(
		MenuItem "Bring &Up To Date"
			[ MenuShortKey 'U'
			, MenuModsFunction (shiftfun (noLS (pm_upto False)) (noLS (pm_upto True)))
			, MenuId (projIds!!3)
			, MenuSelectState Unable
			]
	:+: MenuItem "Update and &Run"
			[ MenuShortKey 'R'
			, MenuModsFunction (shiftfun (noLS pm_exec) (noLS pm_run))
			, MenuId (projIds!!4)
			, MenuSelectState Unable
			]
	:+:	MenuItem "Run"
			[ MenuFunction (noLS pm_run)
			, MenuId (projIds!!5)
			, MenuSelectState Unable
			]
	:+: MenuSeparator []
	:+: MenuItem "&Set Main Module"
			[ MenuFunction (noLS pm_set)
			, MenuId (projIds!!10)
			, MenuSelectState Unable
			]
	:+: MenuItem "&Project Options..."
			[ MenuFunction (noLS projectOptions)
			, MenuId (projIds!!12)
			, MenuSelectState Unable
			]
	:+: MenuSeparator []
	:+: MenuItem "Show &Heap Profile"
			[ MenuFunction (noLS shoheapfun)
			, MenuId (projIds!!6)
			, MenuSelectState Unable
			]
	:+: MenuItem "Show &Time Profile"
			[ MenuFunction (noLS shoprofun)
			, MenuId (projIds!!7)
			, MenuSelectState Unable
			]
	:+:	MenuItem "Theorem &Prover Project"
			[ MenuFunction (noLS provefun)
			, MenuId (projIds!!8)
			, MenuSelectState Unable
			]
	:+: MenuItem "Theorem Prover Module"
			[ MenuSelectState Unable
			, MenuId (projIds!!9)
			]
	:+: MenuSeparator []
	:+: ProjListMenu mProjectId mPrListId mPrRecId
	)
	[ MenuId mProjectId
	]
where
	shiftfun fun1 fun2 mods (ls,ps)
		| not mods.shiftDown
			= fun1 (ls,ps)
		= fun2 (ls,ps)

moduleMenu {md_cmp,md_chk,md_gen,md_cst,md_est} =
	Menu "&Module"
	(	MenuItem "&Compile"
			[ MenuShortKey 'K'
			, MenuFunction (noLS pm_compile)
			, MenuId md_cmp
			, MenuSelectState Unable
			]
	:+: MenuItem "Check Syntax"
			[ MenuShortKey 'Y'
			, MenuFunction (noLS pm_check)
			, MenuId md_chk
			, MenuSelectState Unable
			]
	:+: MenuItem "Generate &Assembly"
			[ MenuFunction (noLS pm_gen_asm)
			, MenuId md_gen
			, MenuSelectState Unable
			]
	:+: MenuSeparator []
	:+: MenuItem "Module Options..."
			[ MenuFunction (noLS pm_copt)
			, MenuId md_cst
			, MenuSelectState Unable
			, MenuShortKey 'J'								// 'Fix' ctrl-J -> newline bug.
			]
/*
	:+:	MenuItem "Editor Settings..."
			[ MenuShortKey 'J'
			, MenuFunction (noLS optionsFontAndTabs) 
			, MenuId md_est
			, MenuSelectState Unable
			]
*/
	)
	[]

optionsMenu mOptionsId
	= Menu "&Defaults"
		(	MenuItem "Module Defaults..."
				[ MenuFunction (noLS pm_coprefs)
				]
		:+: MenuItem "Project Defaults..."
				[ MenuFunction (noLS projectDefaults)
				]
		:+: MenuSeparator 	[]
		:+: SubMenu "Window Settings"
		(	MenuItem "Editor Colours..."
				[ MenuFunction (noLS editColours)
				]
		:+:	MenuItem "Editor Settings..."
				[ MenuFunction (noLS defaultFontAndTabs)
				]
		:+: MenuItem "Project Window..."
				[ MenuFunction (noLS projwinOptions)
				]
		:+: MenuItem "Types Window..."
				[ MenuFunction (noLS typewinColours)
				]
// winOnly?
		:+: MenuItem "Console Window..."
				[ MenuFunction (noLS conswinColours)
				]
// ...
		:+: MenuItem "Search Window..."
				[ MenuFunction (noLS src_options)
				]
		:+: MenuItem "Error Window..."
				[ MenuFunction (noLS err_options)
				]
		) []
		:+:	MenuItem "Key Mapping..."	[ MenuFunction (noLS optionsKeyMapping)]
		:+: MenuItem "IDE Options..."
			[ MenuFunction (noLS (ideOptionsDialog))
			]
		)
		[ MenuId mOptionsId
		]

//--

openfiles [] ps = ps
openfiles [file:files] ps
	# ps = ide_open file ps
	# ps = openfiles files ps
	= ps
where
	ide_open :: !String !(PSt *General) -> (PSt *General)
	ide_open file ps
		| IsPrjPathname file
			= pm_open_path file ps
		= ed_open_path file ps

//--

ed_open :: !Modifiers !(PSt *General) -> (PSt *General)
ed_open {shiftDown} ps
	// ask user for a file to open
	# (maybeString, ps) = selectInputFile ps
	| isNothing maybeString
		= ps
	# pathname		= fromJust maybeString
	// look if project; if so open as project
	| (IsPrjPathname pathname) && (not shiftDown)		// if shift-down always open as text
		= pm_open_path pathname ps
	= ed_open_path pathname ps

Quit :: !String !(PSt *General) -> (PSt *General)
Quit prefspath ps
	= ed_ask_save_all True cont ps
where
	cont ps
		// save project
		# (prefs,ps)			= getPrefs ps
		# (twi,ps)				= accPLoc getTypeWinInfo ps
// winOnly
		# (cwi,ps)				= accPLoc getConsWinInfo ps

		# (errinf,ps)			= getErrInfo ps
		# err_prefs				= err_shut errinf

		# ((proj_pos,proj_siz),ps)
								= pm_get_projwin_possiz ps

		# (fbi,ps)				= getFBI ps
		# srcfdef				= getFontDef fbi.src_font
		# srcfname				= srcfdef.fName
		# srcfsize				= srcfdef.fSize

		# (fhId,ps)				= getFHI ps
		# ((rep,file_hist),ps)	= syncSend2 fhId "" ps
		# file_hist				= if ((rep == SendOk) && isJust file_hist) (fromJust file_hist) []

		# (phId,ps)				= getPHI ps
		# ((rep,proj_hist),ps)	= syncSend2 phId "" ps
		# proj_hist				= if ((rep == SendOk) && isJust proj_hist) (fromJust proj_hist) []
		# src_prefs				=
							 		{ src_pos		= fbi.src_offset
							 		, src_siz		= fbi.src_size
							 		, src_fname		= srcfname
							 		, src_fsize		= srcfsize
							 		, src_forc		= fbi.src_forg
							 		, src_bacc		= fbi.src_back
							 		}
		# prefs					=
							 		{ prefs
							 		& typ_prefs		= TW_GetInf twi
// winOnly
							 		, con_prefs		= getConPrefs cwi
									, err_prefs		= err_prefs
									, src_prefs		= src_prefs
							 		, prj_prefs.proj_pos		= proj_pos
							 		, prj_prefs.proj_siz		= proj_siz
									, proj_hist		= ListToStrictList proj_hist
									, file_hist		= ListToStrictList file_hist
							 		}
		# ps			= savePrefs prefspath prefs ps
		# ps			= pm_save ps
// macOnly?
		# ps			= appPIO (QuitCleanCompiler) ps
		= closeProcess ps
		
//--- Window rotating

nextWindow :: *(PSt .a) -> *PSt .a
nextWindow ps
	# (stack,ps)	= accPIO getWindowsStack ps
	| isEmpty stack = ps
	# [h:t]			= stack
	| isEmpty t = ps
	# f				= last t
	# new			= hd t
	# ps			= stackWindow h f ps
	# ps			= setActiveWindow new ps
	= ps

prevWindow :: *(PSt .a) -> *PSt .a
prevWindow ps
	# (stack,ps)	= accPIO getWindowsStack ps
	| isEmpty stack = ps
	# [_:t]			= stack
	| isEmpty t = ps
	# f				= last t
	# ps			= setActiveWindow f ps
	= ps


//--- editor routines

ide_menu_close mods ps
	| mods.shiftDown
		= ide_close_all ps
	= ide_close ps
	
ide_close ps
	#	(mi,ps)		= accPIO getActiveWindow ps
	| isNothing mi
		= ps
	#	win			= fromJust mi
	#	(isEdit,ps) = isEditWin win ps
	| isEdit
		= ed_close win ps
	# (done,ps) = ew_maybe_close win ps
	| done
		= ps
	# (done,ps) = tw_maybe_close win ps
	| done
		= ps
	# (done,ps) = sw_maybe_close win ps
	| done
		= ps
	// still need to do message win...
	= ps

ide_close_all pstate
	= ed_ask_save_all True id pstate

ide_save mods ps
	| mods.altDown || mods.optionDown
		= ide_save_as mods ps
	| mods.shiftDown
		= ed_save_all ps					// also save project?
	#	(mi,ps)		= accPIO getActiveWindow ps
	| isNothing mi
		= ps
	#	win			= fromJust mi
	#	(isEdit,ps) = isEditWin win ps
	| isEdit
		# (_,ps)	= ed_save win ps
		= ps
	# (isProj,ps)	= isProjWin win ps
	| isProj
		= pm_save ps
	= ps

ide_save_as mods ps
	#	(mi,ps)		= accPIO getActiveWindow ps
	| isNothing mi
		= ps
	#	win			= fromJust mi
	#	(isEdit,ps) = isEditWin win ps
	# (isProj,ps)	= isProjWin win ps
	| mods.shiftDown
		| isEdit
			= ed_save_copy_as ps
		| isProj
			= pm_save_copy_as ps
		= ps
	| isEdit
		= ed_save_as ps
	| isProj
		= pm_save_as ps
	= ps

//--

ed_open_sel openImp mods ps
	| mods.shiftDown
		= open_dlog ps
	# (wId,ps) = accPIO getActiveWindow ps
	| isNothing wId
		= open_dlog ps
	# wId = fromJust wId
	# (res,ps) = maybe_type_win_message wId msgGetSelection ps
	| isJust res
		#	(sel,_)			= fromJust res
		| sel == "" || not (CleanModId sel)
			= open_dlog ps
		#	sel				= (if openImp MakeImpPathname MakeDefPathname) sel
		= OpenModule sel emptySelection ps
	#	(msel,ps)	= sendToActiveWindow msgGetSelection ps
	| isNothing msel
		= open_dlog ps
	#	(sel,_)			= fromJust msel
	| sel == "" || not (CleanModId sel)
		= open_dlog ps
	#	sel				= (if openImp MakeImpPathname MakeDefPathname) sel
	= OpenModule sel emptySelection ps

where
	open_dlog pstate
		#	(dlogId,pstate)		= openId pstate
			(textId,pstate)		= openId pstate
			(okId,pstate)		= openId pstate
			(cancelId,pstate)	= openId pstate
			(_,pstate)			= openModalDialog Void (dialog dlogId textId okId cancelId) pstate
		= pstate
	dialog dlogId textId okId cancelId = Dialog "Open Module"
				(	EditControl (if openImp ".icl" ".dcl") (PixelWidth 200) 1
					[ ControlId textId
					, ControlKeyboard filterReturnKeys Able (\_ -> noLS (dfun True))
					]
				:+:	ButtonControl "Cancel"
					[ ControlPos (Left,zero)
					, ControlFunction (noLS (closeWindow dlogId))
					, ControlId cancelId
					, ControlWidth (ContentWidth "Cancel")
					]
				:+:	ButtonControl "OK"
					[ ControlFunction (noLS (dfun False))
					, ControlId okId
					, ControlWidth (ContentWidth "Cancel")
					]
				)
				[ WindowId dlogId
				, WindowOk okId
				, WindowCancel cancelId
				, WindowClose (noLS (closeWindow dlogId))
				]
	where
		dfun strip ps
			#	(wdef,ps)		= accPIO (getWindow dlogId) ps
			| isNothing wdef
				= closeWindow dlogId ps		// shouldn't be possible
			#	wdef			= fromJust wdef
				(ok,text)		= getControlText textId wdef
			| not ok || isNothing text
				= closeWindow dlogId ps		// shouldn't be possible
			#	text			= fromJust text
			#!	ps				= closeWindow dlogId ps
			#	text			= if strip
									{c \\ c <- [c \\ c <-: text | c <> '\n']}
									text
			= OpenModule text emptySelection ps

ed_open_other pstate
	#	(mpath,pstate) = sendToActiveWindow msgGetPathName pstate
	| isNothing mpath = pstate
	#	path			= fromJust mpath
	| IsDefPathname path
		#	path		= RemoveSuffix path
			path		= MakeImpPathname path
			pstate		= ed_open_path path pstate
		= pstate
	| IsImpPathname path
		#	path		= RemoveSuffix path
			path		= MakeDefPathname path
			pstate		= ed_open_path path pstate
		= pstate
	= pstate

OpenModule :: !.Modulename !.Selection !*(PSt General) -> *PSt General
OpenModule pathname sel ps
	# (syspaths,ps)			= getCurrentPaths ps
	# (prj,ps)				= getProject ps
	# prjpaths				= PR_GetPaths prj
	# srcpaths				= AppendLists prjpaths syspaths
	# srcpaths				= case IsABCPathname pathname of
		True	-> Map MakeSystemPathname srcpaths
		_		-> srcpaths
	# ((ok,fullpath),ps)	= accFiles (SearchDisk False pathname srcpaths) ps
	| not ok
		= okNotice
			[ "Clean Project Manager"
			, "Could not find file:"
			, pathname
			] ps
	# fullpath`				= GetLongPathName fullpath
	= ed_open_path_sel fullpath` sel ps

//--- edit menu stuff

iniEditLS =
	{ zfun = ed_Undo
	, xfun = ed_Cut
	, cfun = ed_Copy
	, vfun = ed_Paste
	}
recfun MGet (ls,ps)		= (ls,(ls,ps))
recfun (MSet ss) (ls,ps)	= (ss,(ss,ps))

editMenu altgr_workaround mEditId editRecId mFileSaveId mFileRevertId {mn_und, mn_cut, mn_cpy, mn_pst, mn_clr, mg_edt, searchIds} iniClip = Menu "&Edit"
		(	MenuItem "&Undo"
				[ MenuShortKey 'Z'
				, MenuFunction (\(ls,ps)->(ls,ls.zfun ps))
				, MenuSelectState Unable
				, MenuId mn_und
				]
		:+:	MenuItem "Cu&t"
				[ MenuShortKey 'X'
				, MenuFunction (\(ls,ps)->(ls,ls.xfun ps))
				, MenuId mn_cut
				]
		:+:	MenuItem "&Copy"
				[ MenuShortKey 'C'
				, MenuFunction (\(ls,ps)->(ls,ls.cfun ps))
				, MenuId mn_cpy
				]
		:+:	MenuItem "&Paste"
				[ MenuShortKey 'V'
				, MenuFunction (\(ls,ps)->(ls,ls.vfun ps))
				, MenuId mn_pst
				]
		:+: Receiver2 editRecId recfun []
		:+:	MenuItem "Cl&ear"			[ MenuId mn_clr,	MenuFunction (noLS ed_Clear) ]
		:+: MenuSeparator []
		:+: MenuItem "Shift &Left"
			[ MenuId (mg_edt!!0)
			, MenuFunction (noLS shift_selection_left)
			: if altgr_workaround [] [MenuShortKey '[']
			]
		:+: MenuItem "Shift &Right"
			[ MenuId (mg_edt!!1)
			, MenuFunction (noLS shift_selection_right)
			: if altgr_workaround [] [MenuShortKey ']']
			]
		:+: MenuItem "&Balance"			[ MenuId (mg_edt!!2), MenuShortKey 'B', MenuFunction (noLS ed_Balance)]
		:+: MenuItem "Select &All"		[ MenuId (mg_edt!!3), MenuShortKey 'A', MenuFunction (noLS ed_SelectAll)]
		:+:	MenuItem "&Detab"			[ MenuId (mg_edt!!4), MenuFunction (noLS detabfun)]
		:+: MenuItem "Add Prefix"		[ MenuId (mg_edt!!5), MenuFunction (noLS add_prefix_selection)]
		:+: MenuItem "Remove Prefix"	[ MenuId (mg_edt!!6), MenuFunction (noLS rem_prefix_selection)]
		:+: MenuItem "Change Prefix..."	[ MenuId (mg_edt!!7), MenuFunction (noLS change_prefix_dlog)]
		:+: MenuSeparator []
		:+: MenuItem "Next &Window"
			[ MenuModsFunction stackfun
			: if altgr_workaround [] [MenuShortKey '\\']
			]
		:+: clipMenuItems mFileSaveId mFileRevertId iniClip
// Wrap...
//		:+: MenuItem "Wrap1320" [MenuShortKey '`', MenuFunction (noLS wrap_preprocessor)]
// ...Wrap
		)
		[ MenuId (mEditId)
		]
where
	stackfun mods (ls,ps)
		| mods.shiftDown
			= (ls,prevWindow ps)
		= (ls,nextWindow ps)

	detabfun ps
		// Add 'Are you sure dialog' ?
		# (_,ps)			= sendToActiveWindow msgDetab ps
		= ps

//--

ed_Undo ps
	# (_,ps)		= sendToActiveWindow msgUndo ps
	= mb_update_undoinfo ps

ed_Paste ps
	# (_,ps)		= sendToActiveWindow msgPaste ps
	= mb_update_undoinfo ps

ed_Copy pstate
	# (res,pstate)		= sendToActiveWindow msgCopy pstate
	| isNothing res
		# (win,pstate)	= accPIO (getActiveWindow) pstate
		| isNothing win
			= pstate
		# win			= fromJust win
		# (_,pstate)	= maybe_type_win_message win msgCopy pstate
		= mb_update_undoinfo pstate
	= mb_update_undoinfo pstate

ed_Cut pstate
	# (res,pstate)		= sendToActiveWindow msgCut pstate
	| isNothing res
		# (win,pstate)	= accPIO (getActiveWindow) pstate
		| isNothing win
			= pstate
		# win			= fromJust win
		# (_,pstate)	= maybe_type_win_message win msgCopy pstate
		= mb_update_undoinfo pstate
	= mb_update_undoinfo pstate

ed_Clear ps
	# (_,ps)	= sendToActiveWindow msgClear ps
	= mb_update_undoinfo ps

ed_SelectAll ps
	# (_,ps)	= sendToActiveWindow msgSelectAll ps
	= mb_update_undoinfo ps

ed_Balance ps
	# (_,ps)	= sendToActiveWindow msgBalance ps
	= mb_update_undoinfo ps

//==== 13/20 transition hack...
// Wrap...
/*
import EdVisualText,EdVisualCursor,EdSelection,StrictList,EdCommon

wrap_preprocessor ps
	# (_,ps)	= sendToActiveWindow wrap ps
	= mb_update_undoinfo ps

wrap =
	getText									>>>= \text ->
	getSelection							>>>= \selection ->
	getNeedSave							>>>= \needsave ->
	IF (isEmptySelection selection)
	THEN
	  ( skip )
	ELSE
	  (
		let orderedSelection	= orderSelection selection
			(ofragment, _)		= getTextFragment orderedSelection text
			fragment			= if (slLast ofragment == "") (slInit ofragment) ofragment
		    nfragment			= slAppend (slAppend (SCons "//1.3" fragment) (SCons "//3.1" (SCons "/*2.0" fragment))) (SCons "0.2*/" (SCons "" SNil))
			position			= orderedSelection.start
			newPosition			= newCursorPos position nfragment
		in
		vDoCursorSafe (
			vRemoveSelection					>>>
			mRemoveSelection										>>>
			vInsertText position nfragment		>>>
			vChangeSelectionTo {start=newPosition,end=newPosition}	>>>
			mChangeSelectionTo {start=newPosition,end=newPosition}	>>>
			vCenterCursor
			)								>>>
		setNeedSave True					>>>
		setUndoInfo
			{ state = Undo
			, action = " Wrap"
			, uninfo = PasteInfo nfragment ofragment selection position needsave
			}
	  )
where
	newCursorPos :: !Position !TextFragment -> Position
  	newCursorPos pos=:{col, row} SNil
  	  = pos
  	newCursorPos {col, row} (SCons string SNil)
  	  = {col = col + size string, row = row }
  	newCursorPos {col, row} strings
  	  = { col = size (slLast strings)
  	    , row = row + slLength strings - 1
  	    }
*/
// ...Wrap
