definition module EdCommon

from StdPSt		import PSt, IOSt
from EdState	import Editor, EditorState
from EdMonad	import EditMonad, EditState
from EdSelection	import Selection, Position, ColumnNr, LineNr

:: *PLocState :== MyEditorState
:: *MyEditorState = MES EditorState

instance Editor MyEditorState

mRemoveSelection :: EditMonad (PSt *MyEditorState) nothing
mChangeSelectionTo	:: Selection				->	EditMonad (PSt *MyEditorState)	nothing
controlDoubleClick :: !.Bool !.Position -> .(!*(.EditState,*PSt PLocState) -> *(a,*(EditState,*PSt PLocState)));
