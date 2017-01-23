/*
 * EdText.dcl: operations on text areas (extends EdLineText)
 */

definition module EdText

//1.3
from StdString	 import String
//3.1
import StdMaybe
from EdSelection import Selection, Position, ColumnNr, LineNr
import EdLineText

getTextFragment	:: !Selection				!Text -> (!TextFragment, !Text)
removeText		:: !Selection				!Text -> (Maybe Int,		 Text)
insertText		:: !Position !TextFragment	!Text -> (Maybe Int,		 Text)
replaceText		:: !Selection !TextFragment !Text -> !Text
appendText		:: !TextFragment			!Text -> Text

// getSelection:	returns the text fragment that is denoted
//					by the selection argument. The text is also returned, 
//					because in some implementations it may change.
//					E.g. the current implementation 
//					maintains a focus on the text to speed up operations
//					that occur nearby each other (very common in editing).
// removeSelection: removes the part of the text that is indicated 
//					by the selection.
// insertText:		inserts a text fragment in a text at a given position.
