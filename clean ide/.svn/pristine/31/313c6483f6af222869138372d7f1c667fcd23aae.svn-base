definition module EdClient

//*********************************************************************************
// Original Clean Library Software Module
// Written for Clean version  : 1.3
// Written for I/O version    : 1.2
// Author                     : Diederik van Arkel
// Date                       :
// Last Modified by           :
// Date                       :
// Copyright                  : 1999 Hilt - High Level Software Tools B.V.
//                            : University of Nijmegen
// e-mail                     : clean@cs.kun.nl or rinus@hilt.nl
//*********************************************************************************
// It is allowed to modify this module for your own purposes but it is NOT allowed
// to (re)distribute the code or the modified code in ANY form without written
// permission.
//*********************************************************************************

/*
 * EdClient.dcl: only this module should be imported by users of the editor
 */

import StdMaybe, StdId, StdPSt, StdPicture, StdPrint
from EdState	import Editor, EditorState
from EdMessage	import EditId
from EdLineText	import Text
import EdPosition
from EdSelection import Selection, emptySelection, lineSelection
from EdMonad	import UndoState, EditMonad, EditState, StateM, getPathName
import IdeState

:: EditAction l a :== EditMonad (PSt l) a

isEditWin :: Id *(PSt *a) -> *(Bool,*PSt *a) | Editor a
				  
// "Remote method invocations". The destination is denoted by a window identifier.

message :: !Id !.(EditAction *b .c) !*(PSt *b) -> *(Maybe .c,*PSt *b) | Editor b
sendToActiveWindow :: .(EditAction *b .c) !*(PSt *b) -> *(Maybe .c,*PSt *b) | Editor b

// Messages

msgSave				::								EditAction General (Maybe String)
msgSaveTo			:: !String ->					EditAction General (Maybe String)

msgSetFont			:: Font	->						EditAction *l nothing
msgGetFont			::								EditAction *l Font

msgSetTabs			:: !(Int,Bool,Bool) ->			EditAction *l nothing
msgGetTabs			::								EditAction *l (Int,Bool,Bool)

msgCopy				::								EditAction *l nothing
msgCut				::								EditAction General nothing
msgPaste			::								EditAction General nothing
msgClear			::								EditAction General nothing
msgSelectAll		::								EditAction General nothing

msgUndo				::								EditAction General nothing

msgBalance			::								EditAction General nothing
msgGetUndoState		::								EditAction *l (UndoState,String)
msgGetPathName		::								EditAction *l String
msgSetPathName		:: String ->					EditAction *l nothing
msgGetNeedSave		::								EditAction *l Bool
msgSetNeedSave		:: Bool ->						EditAction *l nothing
msgGetText			::								EditAction *l Text
msgSetText			:: !Text ->						EditAction *l nothing
msgRevertText		:: !Text ->						EditAction General nothing
msgGetSelection		::								EditAction *l (String,Selection)
msgReplaceSelection	:: String ->					EditAction General nothing
msgChangeSelection	:: Selection -> 				EditAction General nothing
msgScrollToCursor	::								EditAction *l nothing
msgScrollToLine		:: LineNr ->					EditAction General nothing
msgPrint			:: !PrintSetup ->				EditAction *l PrintSetup
msgDetab			::								EditAction *l nothing

:: FRInfo =
	{ fr_pos			:: !Selection
	, fr_search			:: !String
	, fr_replace		:: !String
	, fr_ignore_case	:: !Bool
	, fr_match_words	:: !Bool
	, fr_backwards		:: !Bool
	, fr_wraparound		:: !Bool
	, fr_regexp			:: !Bool
	}

msgFind				:: !FRInfo ->					EditAction General Bool
msgReplace			:: !FRInfo ->					EditAction General Bool
msgReplaceAll		:: !FRInfo ->					EditAction General Int
