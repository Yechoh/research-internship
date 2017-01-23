/*
 * EdKeyboard.dcl: handling keyboard events
 */

definition module EdKeyboard

from StdIOCommon	import KeyboardState, KeyState, SpecialKey, Modifiers, IsRepeatKey, IdFun
from StdPSt			import PSt, IOSt
from EdState		import EditorState
from EdKeyMapping	import KeyMapping
import EdMonad, StdMaybe
import EdCommon

editWindowKeyboard ::
	KeyMapping KeyboardState !(!EditState, !PSt PLocState) -> (!EditState, !PSt PLocState)
// editWindowKeyboard: the keyboard handling function for edit windows

noeditWindowKeyboard ::
	KeyMapping KeyboardState (!EditState, !PSt PLocState) -> (!EditState, !PSt PLocState)
// noeditWindowKeyboard: keyboard handling fun for read-only editor windows
