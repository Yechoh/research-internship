implementation module idehelp

//import StdHtmlHelp
import StdFunc, StdMisc
import StdMenu, StdPStClass, StdSystem
import ExtNotice
import ioutil

//-- export

IDE_VERSION
	:== "v2.0 build 2001-08-28 "
	// want link(?) date/time to be automatically entered...
	+++. PLATFORM
	+++. CLEAN_VERSION

PLATFORM
	= case toInt '\n' of
		13	-> "Mac "
		10	-> "Win "
		_	-> "??? "
		
CLEAN_VERSION
	# clean	= "(1.3)"

// 2.0 only...
/*
// /*
*/
	# clean = "(2.0)"
// */

	= clean

//--

initHelpMenu :: Id !*(PSt .b) -> *PSt .b
initHelpMenu wId ps
	# (_,ps) = openMenu undef (helpMenu wId) ps
	= ps

//-- local

bitmapname		= case toInt '\n' of
					13 -> applicationpath ":bitmaps:aboutIDE.pict"
					10 -> applicationpath "bitmaps//aboutIDE.bmp"
					_ -> abort "idehelp: unknown platform"

idehelpname		= applicationpath "idehelp"
idehelptopic	= "general.htm"

helpMenu :: Id -> Menu (:+: .MenuItem .MenuItem) .a *(PSt .b )
helpMenu wId
	= Menu "&Help"
		(	MenuItem "&About..." [MenuFunction (noLS (about wId))]
		:+:	MenuItem "&Help..." [MenuFunction (noLS (help wId)),MenuSelectState Unable]
		)
		[
		]

help wId ps
//	# ps = htmlHelpTopic (idehelpname+++".chm::/"+++idehelptopic) wId ps
	= ps

about wId ps
	# (wId,ps)	= openId ps
	# (bmap,ps)	= accFiles (openBitmap bitmapname) ps
	| isNothing bmap
		= openNotice (Notice [bitmapname+++" bitmap unavailable."] (NoticeButton "OK" id) []) ps
	# bmap		= fromJust bmap
	# (cId,ps)	= openId ps
	# (_,ps)	= openModalDialog dloc (ddef bmap cId wId) ps
	= ps
where
	dloc = 0
	ddef bmap cId wId
		# bitmapSize = 	getBitmapSize bmap
		# bitmapLook = \_ {newFrame} -> drawAt {x=newFrame.corner1.x+10,y=newFrame.corner2.y-20} IDE_VERSION o draw bmap
		= Dialog "About the Clean IDE"
						( CustomControl bitmapSize bitmapLook [ControlId cId])
						[ WindowClose (noLS (closeWindow wId))
						, WindowId wId
						, WindowHMargin 0 0
						, WindowVMargin 0 0
						]
