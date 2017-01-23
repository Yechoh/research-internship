/*
 * EdState.dcl: the global state of the editor process
 */

definition module EdState

from StdId			import Id
from StdPSt			import PSt, IOSt
from StdMaybe		import Maybe
from EdKeyMapping	import KeyMapping
from EdMonad		import EditState
from EdMessage		import EditId

class Editor env
where
	getEditorState :: !*env -> *(!EditorState,!*env)
	setEditorState :: !EditorState !*env -> *env

instance Editor (PSt *p) | Editor p

:: EditorState

initEditorState		:: !KeyMapping					->	EditorState

findReceiver		:: !Id			!EditorState	->	Maybe EditId
addReceiver			:: Id EditId	!EditorState	->	EditorState
removeReceiver		:: Id			!EditorState	->	EditorState

getKeyMapping		::				!EditorState	->	KeyMapping
setKeyMapping		:: KeyMapping	!EditorState	->	EditorState

