implementation module edoptions

/*

edoptions :- provides the menu items to 

* set/read/save the keymapping for the editor
* set font & tab settings for the editor
* set font for the types window

*/

import StdTuple, StdEnum, StdList, StdFunc, StdMisc
import StdMenu, StdWindow, StdId, StdPSt, StdControl
import StdClipboard, StdControlReceiver
import EdKeyMapping, EdState, EdClient, EdKeyboard
import IDE, IdeState, ExtNotice
import colorpickcontrol
import ioutil, morecontrols
import typewin, colourclip
import PmPath

//-- public

optionsKeyMapping :: !*(PSt *b) -> *PSt *b | Editor b;
optionsKeyMapping ps
	#	(ed,ps)			= getEditorState ps
		keyMapping		= getKeyMapping ed
		ps				= configureKeyMapping keyMapping myset ps
	= ps  
where
	myset keymap ps
		#	(ed,ps)			= getEditorState ps
			ed				= setKeyMapping keymap ed
			ps				= setEditorState ed ps
		= ps

//--

lisFixedWidth :: .FontName *Picture -> (Bool,*Picture)
lisFixedWidth fontname env
  # ((ok,font),   env) = openFont {fName=fontname,fSize=12,fStyles=[]} env
  | not ok = (ok,env)
  # (wide,	 env) = getFontCharWidth font 'M' env
	(narrow, env) = getFontCharWidth font 'i' env
  = (wide == narrow, env)

lfilter :: [.Bool] [.a] -> [.a];
lfilter [True:r] [a:x]	= [a:lfilter r x]
lfilter [_:r] [_:x] = lfilter r x
lfilter _ _				= []

//--

:: ECLS = {ids::[Id],act::Int,cls::[Colour],mfs::EditMenuLS General}

froot txt ps
	# (id,ps) = accPIO openId ps
	# (_,ps) = openModalDialog 0 (Dialog txt NilLS [WindowClose (noLS (closeWindow id)),WindowId id]) ps
	= ps

editColours :: !*(PSt *General) -> *(PSt *General)
editColours ps
	# (prefs,ps)		= getPrefs ps
	# (rgbid,ps)		= openRGBId  ps
	# (lsid,ps)			= openRId ps
	# (wId,ps)			= openId ps
	# (okId,ps)			= openId ps
	# (cancelId,ps)		= openId ps
	# (ids,ps)			= openIds 21 ps
	# cls =
							[ prefs.syncols.textColour
							, prefs.defcols.textColour
							, prefs.impcols.textColour
							, prefs.syncols.tabColour
							, prefs.defcols.tabColour
							, prefs.impcols.tabColour
							, prefs.syncols.commentColour
							, prefs.defcols.commentColour
							, prefs.impcols.commentColour
							, prefs.syncols.stringColour
							, prefs.defcols.stringColour
							, prefs.impcols.stringColour
							, prefs.syncols.charColour
							, prefs.defcols.charColour
							, prefs.impcols.charColour
							, prefs.syncols.backgroundColour
							, prefs.defcols.backgroundColour
							, prefs.impcols.backgroundColour
							, prefs.syncols.keywordColour
							, prefs.defcols.keywordColour
							, prefs.impcols.keywordColour
							]
	# wloc				= {act=0,ids=ids,cls=cls,mfs={zfun=froot "options - Z",xfun=froot "options - X",cfun=id,vfun=id}}
	# (siz,ps)			= controlSize
							(ColourBoxControl`` rgbid lsid cls ids 1 Nothing)
							False
							(Just (0,0))
							(Just (0,0))
							(Just (0,0)) ps
	# buttonWidth = ContentWidth "Cancel"
	# wdef				= Dialog "Editor Colours"
							(	RGBColourPickControl` rgbid (prefs.syncols.textColour) (idslook wloc) Nothing
							:+: ButtonControl "&Copy"
								[ ControlFunction (copyFun wId)
								, ControlWidth buttonWidth
								]
							:+: ButtonControl "&Paste"
								[ ControlFunction (pasteFun wId rgbid)
								, ControlPos (BelowPrev,zero)
								, ControlWidth buttonWidth
								]
							:+: ButtonControl "&Apply"
								[ ControlFunction (applyFun rgbid lsid)
								, ControlPos (BelowPrev,zero)
								, ControlWidth buttonWidth
								]
							:+: ButtonControl "Ca&ncel"
								[ ControlFunction (cancelFun wloc wId)
								, ControlId cancelId
								, ControlPos (BelowPrev,zero)
								, ControlWidth buttonWidth
								]
							:+: ButtonControl "&OK"
								[ ControlFunction (okFun rgbid lsid wId)
								, ControlId okId
								, ControlPos (BelowPrev,zero)
								, ControlWidth buttonWidth
								]
							:+: TextControl ".xxx" [ControlWidth (PixelWidth siz.w),ControlPos (Left,zero)]
							:+: TextControl ".dcl" [ControlWidth (PixelWidth siz.w)]
							:+: TextControl ".icl" [ControlWidth (PixelWidth siz.w)]
							:+: ColourBoxControl`` rgbid lsid cls ids 0 (Just (Left,zero))
							:+: ColourBoxControl`` rgbid lsid cls ids 1 Nothing
							:+: ColourBoxControl`` rgbid lsid cls ids 2 Nothing
							:+: TextControl "Text" []

							:+: ColourBoxControl`` rgbid lsid cls ids 3 (Just (Left,zero))
							:+: ColourBoxControl`` rgbid lsid cls ids 4 Nothing
							:+: ColourBoxControl`` rgbid lsid cls ids 5 Nothing
							:+: TextControl "Tabs" []

							:+: ColourBoxControl`` rgbid lsid cls ids 6 (Just (Left,zero))
							:+: ColourBoxControl`` rgbid lsid cls ids 7 Nothing
							:+: ColourBoxControl`` rgbid lsid cls ids 8 Nothing
							:+: TextControl "Comments" []

							:+: ColourBoxControl`` rgbid lsid cls ids 9 (Just (Left,zero))
							:+: ColourBoxControl`` rgbid lsid cls ids 10 Nothing
							:+: ColourBoxControl`` rgbid lsid cls ids 11 Nothing
							:+: TextControl "Strings" []

							:+: ColourBoxControl`` rgbid lsid cls ids 12 (Just (Left,zero))
							:+: ColourBoxControl`` rgbid lsid cls ids 13 Nothing
							:+: ColourBoxControl`` rgbid lsid cls ids 14 Nothing
							:+: TextControl "Chars" []

							:+: ColourBoxControl`` rgbid lsid cls ids 15 (Just (Left,zero))
							:+: ColourBoxControl`` rgbid lsid cls ids 16 Nothing
							:+: ColourBoxControl`` rgbid lsid cls ids 17 Nothing
							:+: TextControl "Backgorund" []

							:+: ColourBoxControl`` rgbid lsid cls ids 18 (Just (Left,zero))
							:+: ColourBoxControl`` rgbid lsid cls ids 19 Nothing
							:+: ColourBoxControl`` rgbid lsid cls ids 20 Nothing
							:+: TextControl "Keywords" []
							:+: Receiver lsid lsfun []
							)
							[ WindowPen [PenBack LightGrey]
							, WindowClose 	(cancelFun wloc wId)
							, WindowId wId
							, WindowInit (setBoxCol)
							, WindowOk okId
							, WindowCancel cancelId
							]
	# (_,ps) = openModalDialog wloc wdef ps
	= ps
where
	copyFun wId (ls,ps)
		// get active colour control
		// and put in clipboard
		# cur	= ls.cls!!ls.act
		# ps	= setClipboard [toClipboard (toString cur)] ps
		= (ls,ps)
	pasteFun wId rId (ls,ps)
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
		# ls		= {ls & cls = updateAt ls.act (fromString it) ls.cls}
		# (ls,ps)	= setBoxCol (ls,ps)
		# ps		= setColourBoxColour` rId (clslook ls) ps
		= (ls,ps)
	ColourBoxControl`` rgbid lsid cls ids x p
		= ColourBoxControl` (toRGBColour (cls!!x)) (ids!!x) (mstuff rgbid lsid x) p
	where
		mstuff rgbid lsid x = (mfilter,mfunction rgbid lsid x)
		mfilter (MouseDown _ _ _) = True
		mfilter _ = False
		mfunction rgbid lsid x _ (ls,ps)
			= updateActiveInLS rgbid lsid cont (ls,ps)
		where
			cont (ls,ps)
				# ps	= appPIO (SetColourBox (idslook ls) (toRGBColour(clslook ls))) ps
				# ls	= {ls & act = x}
				# ps	= appPIO (SetColourBox` (idslook ls) (toRGBColour(clslook ls))) ps
				# ps	= setColourBoxId rgbid (idslook ls) ps
				# ps	= setColourBoxColour` rgbid (clslook ls) ps
				= (ls,ps)

	setBoxCol (ls,ps)
		# ps = appPIO (SetColourBox` (idslook ls) (toRGBColour(clslook ls))) ps
		= (ls,ps)
	idslook {ids,act} = ids!!act
	clslook {cls,act} = cls!!act
	toStringC :: !Colour -> String
	toStringC c = toString c
	lsfun f (ls,ps) = f (ls,ps)

	updateActiveInLS rgbid lsid cont3 (ls,ps)
		# ps = getColourBoxColour rgbid cont ps
		= (ls,ps)
	where
		cont col ps
				| isNothing col = ps
				# col = fromJust col
				# (_,ps) = asyncSend lsid (cont2 col) ps
				= ps
		cont2 col (ls,ps)
				# ls	= {ls & cls = updateAt ls.act col ls.cls}
				= cont3 (ls,ps)

	okFun rgbid lsid wId (ls,ps)
		= updateActiveInLS rgbid lsid cont (ls,ps)
	where
		cont (ls,ps)
			# (ls,ps) = apply (ls,ps)
			= (ls, closeWindow wId ps)
	
	cancelFun inils wId (_,ps)
		# (ls,ps) = apply (inils,ps)
		= (ls, closeWindow wId ps)
	
	applyFun rgbid lsid (ls,ps)
		= updateActiveInLS rgbid lsid apply (ls,ps)

	apply (ls,ps)
		# (prefs,ps)	= getPrefs ps
		# syncols` =
							{ prefs.syncols
							& textColour		= ls.cls!!0
							, tabColour			= ls.cls!!3
							, commentColour		= ls.cls!!6
							, stringColour		= ls.cls!!9
							, charColour		= ls.cls!!12
							, backgroundColour	= ls.cls!!15
							, keywordColour		= ls.cls!!18
							}
		# defcols` =
							{ prefs.defcols
							& textColour		= ls.cls!!1
							, tabColour			= ls.cls!!4
							, commentColour		= ls.cls!!7
							, stringColour		= ls.cls!!10
							, charColour		= ls.cls!!13
							, backgroundColour	= ls.cls!!16
							, keywordColour		= ls.cls!!19
							}
		# impcols` =
							{ prefs.impcols
							& textColour		= ls.cls!!2
							, tabColour			= ls.cls!!5
							, commentColour		= ls.cls!!8
							, stringColour		= ls.cls!!11
							, charColour		= ls.cls!!14
							, backgroundColour	= ls.cls!!17
							, keywordColour		= ls.cls!!20
							}
		# prefs				= {prefs & syncols = syncols`, defcols = defcols`, impcols = impcols`}
		# ps				= setPrefs prefs ps
		# (windows,ps)		= accPIO getWindowsStack ps
		# ps				= doall prefs windows ps
		= (ls,ps)
	where
		doall prefs [] ps
			= ps
		doall prefs [win:rest] ps
			// need to ignore special edit windows, ie clipboard & types window...
			# (isclip,ps) = isClipboardWindow win ps
			| isclip
				= doall prefs rest ps
			# (twi,ps) = accPLoc getTypeWinInfo ps
			| isTypeWindow win twi
				= doall prefs rest ps
			# (pn,ps)		= message win (getPathName) ps
			| isNothing pn
				= doall prefs rest ps
			# pn			= fromJust pn
			# cols			= if (IsDefPathname pn) (prefs.defcols) (if (IsImpPathname pn) (prefs.impcols) (prefs.syncols))
			# (_,ps)		= message win (appFontInfo (fi_update cols)) ps
			# ps			= appPIO (updateWindow win Nothing) ps
			= doall prefs rest ps
		fi_update cols fi =
			{ fi
			& syntaxColours =
				{ fi.syntaxColours
				& textColour		= cols.textColour
				, tabColour			= cols.tabColour
				, commentColour		= cols.commentColour
				, stringColour		= cols.stringColour
				, charColour		= cols.charColour
				, backgroundColour	= cols.backgroundColour
				, keywordColour		= cols.keywordColour
				}
			}

defaultColours :: !*(PSt *General) -> *(PSt *General)
defaultColours ps
	# (prefs,ps)		= getPrefs ps
	# (rgbid,ps)		= openRGBId  ps
	# (wId,ps)			= openId ps
	# wloc				= 0
	# col				= case wloc of
								0 -> prefs.syncols.textColour
								1 -> prefs.syncols.tabColour
								2 -> prefs.syncols.commentColour
								3 -> prefs.syncols.stringColour
								4 -> prefs.syncols.charColour
								5 -> prefs.syncols.backgroundColour
								6 -> prefs.syncols.keywordColour
								_ -> abort "edoptions[defaultColours]: unknown ls"
	# wdef				= Dialog "Pick a colour"
							(	RGBColourPickControl rgbid col Nothing
							:+: PopUpControl
									[("Text"		,psel rgbid 0)
									,("Tabs"		,psel rgbid 1)
									,("Comments"	,psel rgbid 2)
									,("Strings"		,psel rgbid 3)
									,("Chars"		,psel rgbid 4)
									,("Background"	,psel rgbid 5)
									,("Keywords"	,psel rgbid 6)
									] wloc []
							:+: ButtonControl "Set" [ControlFunction (cset rgbid)]
							)
							[	WindowPen [PenBack LightGrey]
							,	WindowClose 	(dlogClose wId)
							,	WindowId wId
							]
	# (_,ps) = openModalDialog wloc wdef ps
	= ps
where
	dlogClose wId (ls,ps)
		# ps				= closeWindow wId ps
		= (ls,ps)
	psel rid i (ls,ps)
		# (prefs,ps)	= getPrefs ps
		# col			= case i of
								0 -> prefs.syncols.textColour
								1 -> prefs.syncols.tabColour
								2 -> prefs.syncols.commentColour
								3 -> prefs.syncols.stringColour
								4 -> prefs.syncols.charColour
								5 -> prefs.syncols.backgroundColour
								6 -> prefs.syncols.keywordColour
								_ -> abort "edoptions[defaultColours]: also unknown ls"
		# ps = setColourBoxColour rid col ps
		= (i,ps)
	cset rid (ls,ps)
		# ps = getColourBoxColour rid cont ps
		= (ls,ps)
	where
		cont col ps
			| isNothing col = ps
			# col = fromJust col
			# (prefs,ps)		= getPrefs ps
			# prefs				= {prefs & syncols = sc_update ls prefs.syncols col}
			# ps				= setPrefs prefs ps
			# (windows,ps)		= accPIO getWindowsStack ps
			= doall prefs windows ps

		sc_update 0 sc col = {sc & textColour		= col}
		sc_update 1 sc col = {sc & tabColour		= col}
		sc_update 2 sc col = {sc & commentColour	= col}
		sc_update 3 sc col = {sc & stringColour		= col}
		sc_update 4 sc col = {sc & charColour		= col}
		sc_update 5 sc col = {sc & backgroundColour	= col}
		sc_update 6 sc col = {sc & keywordColour	= col}
		sc_update _ sc col = abort "edoptions[defaultColours:sc_update]: unknown ls"

		doall prefs [] ps
			= ps
		doall prefs [win:rest] ps
			# (pn,ps)		= message win (getPathName) ps
			| isNothing pn
				= doall prefs rest ps
			# pn			= fromJust pn
			| IsDefPathname pn || IsImpPathname pn
				= doall prefs rest ps
			# (_,ps)		= message win (appFontInfo (fi_update prefs)) ps
			# ps = appPIO (updateWindow win Nothing) ps
			= doall prefs rest ps
		fi_update prefs fi =
			{ fi
			& syntaxColours =
				{ fi.syntaxColours
				& textColour		= prefs.syncols.textColour
				, tabColour			= prefs.syncols.tabColour
				, commentColour		= prefs.syncols.commentColour
				, stringColour		= prefs.syncols.stringColour
				, charColour		= prefs.syncols.charColour
				, backgroundColour	= prefs.syncols.backgroundColour
				, keywordColour		= prefs.syncols.keywordColour
				}
			}

lineFun :: Id .Bool *(PSt *b) -> *PSt *b | Editor b;
lineFun window linenumbers ps 
	#	(_, ps)				= message window (setLineNumbers linenumbers) ps
	= ps

syncFun :: Id .Bool *(PSt *b) -> *PSt *b | Editor b;
syncFun window syntaxcolor ps 
	#	(_, ps)				= message window (appFontInfo (\fi->{fi & showSyntax = syntaxcolor})) ps
	# ps = appPIO (updateWindow window Nothing) ps
	= ps

// perform an operation on the font of the given window

fontAction :: Id .(FontDef -> .FontDef) *(PSt *c )-> *(PSt *c) | Editor c;
fontAction window fontChange ps 
	# (font, ps)			= message window msgGetFont ps
	| isNothing font
		= ps
	# font					= fromJust font
	# fontDef				= getFontDef font
	# newFontDef			= fontChange fontDef
	# ((ok, newFont), ps)	= accScreenPicture (openFont newFontDef) ps
	| not ok
		# (newFont, ps)		= accScreenPicture (openDefaultFont) ps
		# (r, ps)			= message window (msgSetFont newFont) ps
		| isNothing r
			= ps
		= ps
	# (r, ps)				= message window (msgSetFont newFont) ps
	| isNothing r
		= ps
	= ps

// perform an operation on the tabs of the given window

formatTabs :: Id .Int *(PSt *b) -> *PSt *b | Editor b;
formatTabs window tabSize pstate
	= tabsAction window changeTabs pstate
where
	changeTabs (t,a,s) = (tabSize,a,s)

autoTabs :: Id .Bool *(PSt *b) -> *PSt *b | Editor b;
autoTabs  window autoTab pstate
	= tabsAction window changeAuto pstate
where
	changeAuto (t,a,s) = (t,autoTab,s)

showTabs :: Id .Bool *(PSt *b) -> *PSt *b | Editor b;
showTabs window showTab pstate
	= tabsAction window changeShow pstate
where
	changeShow (t,a,s) = (t,a,showTab)

tabsAction :: Id .((Int,Bool,Bool) -> (.Int,.Bool,.Bool)) *(PSt *b) -> *PSt *b | Editor b;
tabsAction window tabsChange ps 
	#	(tabs, ps)				= message window msgGetTabs ps
	| isNothing tabs
		= ps
	#	tabs					= fromJust tabs
		newTabs					= tabsChange tabs
		(_, ps)					= message window (msgSetTabs newTabs) ps
	= ps

//--

defaultFontAndTabs :: !*(PSt *General) -> *(PSt *General)
defaultFontAndTabs ps
	# (names, ps) = accPIO (accScreenPicture getFontNames) ps
	// filter fixed width fonts....
	# (fixed,ps) = seqList (map (\f->accPIO (accScreenPicture (lisFixedWidth f))) names) ps
	# names = lfilter fixed names
	# (prefs,ps) = getPrefs ps
	# fontdef = prefs.edwinfont
	# (initabs,iniauto,inishow,iniline,inisync) = prefs.edwintabs
	# fontname = fontdef.fName
	# fontsize = fontdef.fSize
	# fontSizes = [7, 8, 9, 10, 12, 14, 18, 24 ]
	# inistate = (initabs,iniauto,inishow,iniline,inisync)
	# (dialogId,ps)		= openId ps
	# (okId,ps)		= openId ps
	# (tabsId,ps)		= openId ps
	# (cancelId,ps)		= openId ps
	# controls
		=	FontNameSizeControl fontname fontsize names fontSizes fontfun sizefun [ left ]
		:+:	TextControl "Tabs every" [ left ]
		:+:	EditControl (toString initabs) (PixelWidth 30) 1 [ControlKeyboard (const True) Able (\_ -> (tabsfun dialogId tabsId)), ControlId tabsId] 
		:+:	TextControl "characters" []
		:+: CheckControl
			[("Auto Indent"		,Nothing,toMark iniauto,(autofun))
			,("Show Tabs"		,Nothing,toMark inishow,(showfun))
			,("Show LineNrs"	,Nothing, toMark iniline, (linefun))
			,("Syntax Colouring"		,Nothing, toMark inisync, (syncfun))
			]
			(Columns 1)
			[left]
    	:+:	ButtonControl "OK"
			[ ControlId okId
			, ControlFunction (okfun dialogId) 
			, ControlPos (Right, zero)
			, ControlWidth (ContentWidth "Cancel")
			]
    	:+:	ButtonControl "Cancel"
			[ ControlPos (LeftOfPrev, zero) 
			, ControlFunction (cancelfun dialogId inistate fontname fontsize)
			, ControlId cancelId
			] 
    	:+:	ButtonControl "Apply"
			[ ControlPos (LeftOfPrev, zero) 
			, ControlFunction (applyfun)
			] 
	# dialog
		= Dialog "Editor Settings" controls 
	  		[ WindowId dialogId 
	  		, WindowOk okId
	  		, WindowCancel cancelId
	  		, WindowClose (cancelfun dialogId inistate fontname fontsize)
	  		]
	# (_,ps) = openModalDialog inistate dialog ps
	= ps
where
    cancelfun dialogId inistate fontname fontsize (ls,ps)
     	# (prefs,ps) = getPrefs ps
    	# ps = setPrefs {prefs & edwintabs = inistate, edwinfont = {prefs.edwinfont & fName = fontname, fSize = fontsize}} ps
     	# (ls,ps) = apply (ls,ps)
     	= (ls, closeWindow dialogId ps)
    applyfun (ls,ps)
    	= apply (ls,ps)
    okfun dialogId (ls,ps)
    	# (ls,ps) = apply (ls,ps)
    	= (ls, closeWindow dialogId ps)
    left = ControlPos (Left, zero)
    fontfun name (ls,ps)
     	# (prefs,ps) = getPrefs ps
    	# ps = setPrefs {prefs & edwinfont = {prefs.edwinfont & fName = name}} ps
    	= (ls,ps)
    sizefun size (ls,ps)
     	# (prefs,ps) = getPrefs ps
    	# ps = setPrefs {prefs & edwinfont = {prefs.edwinfont & fSize = size}} ps
    	= (ls,ps)
    tabsfun dialogId tabsId ((t,a,s,l,c),ps)
    	# (wstate,ps)	= accPIO (getWindow dialogId) ps
    	| isNothing wstate = ((t,a,s,l,c),ps)
    	# wstate		= fromJust wstate
    	# [(ok,mt):_]	= getControlTexts [tabsId] wstate
    	| not ok = ((t,a,s,l,c),ps)
    	| isNothing mt = ((t,a,s,l,c),ps)
    	# t				= fromJust mt
    	# t				= toInt t
     	# (prefs,ps) = getPrefs ps
    	# ps = setPrefs {prefs & edwintabs = (t,a,s,l,c)} ps
    	= ((t,a,s,l,c),ps)
    autofun ((t,a,s,l,c),ps)
    	# a = not a
     	# (prefs,ps) = getPrefs ps
    	# ps = setPrefs {prefs & edwintabs = (t,a,s,l,c)} ps
    	= ((t,a,s,l,c),ps)
    showfun ((t,a,s,l,c),ps)
    	# s = not s
     	# (prefs,ps) = getPrefs ps
    	# ps = setPrefs {prefs & edwintabs = (t,a,s,l,c)} ps
    	= ((t,a,s,l,c),ps)	
    linefun ((t,a,s,l,c),ps)
    	# l = not l
     	# (prefs,ps) = getPrefs ps
    	# ps = setPrefs {prefs & edwintabs = (t,a,s,l,c)} ps
    	= ((t,a,s,l,c),ps)
    syncfun ((t,a,s,l,c),ps)
    	# c = not c
     	# (prefs,ps) = getPrefs ps
    	# ps = setPrefs {prefs & edwintabs = (t,a,s,l,c)} ps
    	= ((t,a,s,l,c),ps)
	apply (ls,ps)
		# (prefs,ps)		= getPrefs ps
		# (windows,ps)		= accPIO getWindowsStack ps
		# ps				= doall prefs windows ps
		= (ls,ps)
	where
		doall prefs [] ps
			= ps
		doall prefs [win:rest] ps
			// need to ignore special edit windows, ie clipboard & types window...
			# (isclip,ps) = isClipboardWindow win ps
			| isclip
				= doall prefs rest ps
			# (twi,ps) = accPLoc getTypeWinInfo ps
			| isTypeWindow win twi
				= doall prefs rest ps
			# (pn,ps)		= message win (getPathName) ps
			| isNothing pn
				= doall prefs rest ps
			# (t,a,s,l,c)	= prefs.edwintabs
			# fontname		= prefs.edwinfont.fName
			# fontsize		= prefs.edwinfont.fSize
	    	# ps			= formatTabs win t ps
			# ps			= autoTabs win a ps
	    	# ps			= showTabs win s ps
		   	# ps			= lineFun win l ps
	    	# ps			= syncFun win c ps
	    	# ps			= fontAction win (\fontdef->{fontdef & fName = fontname, fSize = fontsize}) ps
			= doall prefs rest ps
