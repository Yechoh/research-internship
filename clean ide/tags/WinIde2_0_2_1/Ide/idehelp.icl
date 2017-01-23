implementation module idehelp

import StdFunc, StdMisc
import StdMenu, StdPStClass, StdSystem
import ExtNotice
import ioutil, UtilIO, IdePlatform
import first_run
import Directory, StdTuple

//-- export

IDE_VERSION
	:== "v2.0 build 2001-12-18 "
	// want link(?) date/time to be automatically entered...
	+++. PLATFORM
	+++. CLEAN_VERSION
	+++. EXE_VERSION

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
	# clean = "(2.0) "
// */

	= clean

EXE_VERSION
	= toString (fst3 GetVNP)

//--

initHelpMenu :: Id !*(PSt .b) -> *PSt .b
initHelpMenu wId ps
	# (mId,ps)	= openId ps
	# (_,ps)	= openMenu Void (helpMenu wId mId) ps
	# ps		= helpItems wId mId ps
	= ps

//-- local

bitmapname		= case toInt '\n' of
					13 -> applicationpath ":bitmaps:aboutIDE.pict"
					10 -> applicationpath "bitmaps\\aboutIDE.bmp"
					_ -> abort "idehelp: unknown platform"

idehelpname		= applicationpath "idehelp"
idehelptopic	= "general.htm"

helpMenu wId mId
	= Menu "&Help"
		(	MenuItem "&About..." [MenuFunction (noLS (about wId))]
		:+:	SubMenu "&Help" NilLS [MenuId mId]
		)
		[
		]

helpItems wId mId ps
	# path					= applicationpath "help"
	# ((ok,path`),ps)		= pd_StringToPath path ps
	| not ok = ps
	# ((err,dir),ps)		= getDirectoryContents path` ps
	| err <> NoDirError = ps
	# items					= map getinfo dir
	= to_menu_items (path+++."\\") items mId ps
where
	getinfo {fileName,fileInfo=fi=:{pi_fileInfo=dummyname=:{isDirectory}}}
		= (isDirectory,fileName)
	
	to_menu_items path [] mId ps = ps
	to_menu_items path [(is_dir,filename):rest] mId ps
		| not is_dir
			# item			= MenuItem filename [MenuFunction (noLS (help path filename wId))]
			# (err,ps)		= openSubMenuElements mId 32000 Void item ps
			= to_menu_items path rest mId ps
		| filename == "." || filename == ".."
			= to_menu_items path rest mId ps
		# ((ok,path`),ps)	= pd_StringToPath (path+++.filename) ps
		| not ok
			= to_menu_items path rest mId ps
		# ((err,dir),ps)	= getDirectoryContents path` ps
		| err <> NoDirError
			= to_menu_items path rest mId ps
		# items				= map getinfo dir		// only need common fileinfo...
		# (mId`,ps)			= openId ps
		# item				= SubMenu filename NilLS [MenuId mId`]
		# (err,ps)			= openSubMenuElements mId 32000 Void item ps
		# ps				= to_menu_items (path+++.filename+++."\\") items mId` ps
		= to_menu_items path rest mId ps

help path file wId ps
	# path		= path +++. file
	# (ret,ps)	= ShellDefault path ps
	= ps

about wId ps
	# (wId,ps)	= openId ps
	# (bmap,ps)	= GetBitmapResource AboutBitmap ps
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
