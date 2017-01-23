definition module PmPrefs

/*
	The IDE Preferences
*/

from StdPictureDef import FontDef, FontName, FontStyle, FontSize
import StdFile, StdIOBasic
import StdPathname
import PmTypes
from UtilStrictLists import List
from PmCompilerOptions import CompilerOptions,ListTypes
from EdMonad import SyntaxColours, Colour

PrefsFileName :== "IDEPrefs"

:: ErrPrefs =									// Error window preferences
	{ err_pos		:: !Vector2
	, err_siz		:: !Size
	, err_fname		:: !String
	, err_fsize		:: !Int
	, err_forc		:: !Colour
	, err_bacc		:: !Colour
	, err_err		:: !Bool
	, err_wrn		:: !Bool
	, err_inf		:: !Bool
	}

:: SrcPrefs =									// Search window preferences
	{ src_pos		:: !Vector2
	, src_siz		:: !Size
	, src_fname		:: !String
	, src_fsize		:: !Int
	, src_forc		:: !Colour
	, src_bacc		:: !Colour
	}	

:: TypPrefs =									// Types window preferences
	{ typewinfont	:: !FontDef
	, typewinpos	:: !Vector2
	, typewinsiz	:: !Size
	, typewinsync	:: !SyntaxColours
	}

:: ConPrefs =									// Console window preferences
	{ conswinfont	:: !FontDef
	, conswinpos	:: !Vector2
	, conswinsiz	:: !Size
	, conswinsync	:: !SyntaxColours
	}

:: PrjPrefs =									// Project preferences
	{ proj_pos		:: !Vector2					// Project window position
	, proj_siz		:: !Size					// Project window size
	, proj_topc		:: !Colour					// Colour settings for project window
	, proj_forc		:: !Colour
	, proj_bacc		:: !Colour
	, proj_font		:: !FontDef					// Font for project window
	, proj_shft		:: !Bool					// True: open dcl default; False: open icl default;
	}

:: Prefs =
	{ pmp_pth		:: !List Pathname
	, pmp_lnk		:: !List Pathname
	, pmp_obj		:: !List Pathname
	, typ_prefs		:: !TypPrefs
	, con_prefs		:: !ConPrefs
	, err_prefs		:: !ErrPrefs
	, src_prefs		:: !SrcPrefs
	, prj_prefs		:: !PrjPrefs

// want to set these per filetype...
	, edwinfont		:: !FontDef
	, edwintabs		:: !(Int,Bool,Bool,Bool,Bool)	// tabsize, autotab, showtabs, showlinenos, showsyncol

	, compopts		:: !CompilerOptions
	, cgenopts		:: !CodeGenOptions
	, linkopts		:: !LinkOptions
	, applopts		:: !ApplicationOptions

	, syncols		:: !SyntaxColours				// default syntax colours
	, defcols		:: !SyntaxColours				// .dcl syntax colours
	, impcols		:: !SyntaxColours				// .icl syntax colours

	, proj_hist		:: !List Pathname				// project history
	, file_hist		:: !List Pathname				// file history
	, switch_close	:: !Bool						// close associated editor windows on project switch
	, show_toolbar	:: !Bool						// enable toolbar
	, enable_prover	:: !Bool						// enable theorem prover
	, be_verbose	:: !Bool						// give extended diagnostics
	, altgr_workaround	:: !Bool					// temp fix to workaround french azerty keyboard menu shortcuts...
	, newline_handling	:: !NewlinePrefs
	}

:: NewlinePrefs
	= LeaveAlone NewlineConvention	// leave alone existing files, use given convention for new files
	| AlwaysUse NewlineConvention	// always save with given newline convention

openPrefs :: !String !*a -> *(!Prefs,!*a) | FileEnv a
savePrefs :: !String Prefs *a -> *a | FileEnv a
