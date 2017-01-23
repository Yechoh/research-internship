/*
 * EdOptionsMenu.dcl: the options menu
 */

definition module EdOptionsMenu

from StdMenu	import Menu, Title, MenuAttribute
from StdPSt		import PSt, IOSt
from EdCommon	import MyEditorState

openOptionsMenu :: !(PSt *MyEditorState) -> PSt *MyEditorState

