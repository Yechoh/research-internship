implementation module idehelp

import StdFunc, StdMisc
import StdMenu, StdPStClass, StdSystem
import ExtNotice
import ioutil, UtilIO, IdePlatform

//-- export

initHelpMenu :: !Id !*(PSt .b) -> *PSt .b
initHelpMenu wId ps
	# (mId,ps)	= openId ps
	# (_,ps)	= openMenu Void (helpMenu wId mId) ps
	# ps		= helpItems wId mId ps
	= ps

//-- local

helpMenu wId mId
	= Menu "&Help"
		(	MenuItem "&About..." [MenuFunction (noLS (about wId))]
		:+:	SubMenu "&Help" NilLS [MenuId mId]
		)
		[
		]

about wId ps
	# (wId,ps)	= openId ps
	# (bmap,ps)	= getAboutBitmap ps
	| isNothing bmap
		= openNotice (Notice ["AboutIDE bitmap unavailable."] (NoticeButton "OK" id) []) ps
	# bmap		= fromJust bmap
	# (cId,ps)	= openId ps
	# (_,ps)	= openModalDialog dloc (ddef bmap cId wId) ps
	= ps
where
	dloc = 0
	ddef bmap cId wId
		# bitmapSize = 	getBitmapSize bmap
		= Dialog "About the Clean IDE"
						( CustomControl bitmapSize bitmapLook [ControlId cId])
						[ WindowClose (noLS (closeWindow wId))
						, WindowId wId
						, WindowHMargin 0 0
						, WindowVMargin 0 0
						]
	where
		bitmapLook _ {newFrame} p
			# p					= draw bmap p
			# (font,p)			= openDefaultFont p
			# ((ok,font`),p)	= openFont {SerifFontDef & fSize = 14} p
			# font``			= if ok font` font
			# p					= setPenFont font`` p
			# p					= drawAt {x=newFrame.corner1.x+10,y=newFrame.corner2.y-20} IDE_VERSION p
			= p

IDE_VERSION
	:==  BUILD_VERSION
	// want link(?) date/time to be automatically entered...
	+++. PLATFORM
	+++. CLEAN_VERSION
	+++. EXE_VERSION

BUILD_VERSION	:== "v2.1.1 build 2005-04-22 "

PLATFORM
	= case toInt '\n' of
		13	-> "Mac "
		10	-> "Win "
		_	-> "??? "
		
CLEAN_VERSION
	# clean	= "(1.3) "

// 2.0 only...
/*
// /*
*/
	# clean = "(2.1.1) "
// */

	= clean
