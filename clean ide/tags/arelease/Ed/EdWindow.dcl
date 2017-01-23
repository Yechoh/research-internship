/*
 * EdWindow.dcl: opening and closing editor windows
 */

definition module EdWindow

//1.3
from StdString		import String
//3.1
from StdWindow		import Title, WindowAttribute
from StdPSt			import PSt, IOSt
from StdId			import Id
from StdPicture		import Font
from StdIOCommon	import MouseState
from EdText			import Text
from EdState		import EditorState,Editor
import EdMonad, StdIOBasic, StdMaybe


// openEditWindow: create a new edit window with the given properties
openEditWindow ::
	Id											// undo menu item id
	.Title										// window title
	String										// file path
	Text										// initial text
	!Font										// initial font
	(Int,Bool,Bool,Bool,Bool)					// tabs
	SyntaxColours								// syntax colours
	Id											// window id
	[.WindowAttribute *(EditState,*PSt *b)]		// initial attributes
	!*(PSt *b) -> !*PSt *b | Editor b;

closeEditWindow ::
	!Id											// window id
	!*(PSt *b) -> *PSt *b | Editor b;

