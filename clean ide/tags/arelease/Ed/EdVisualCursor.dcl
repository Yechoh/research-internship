definition module EdVisualCursor

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
 * EdVisualCursor.dcl: visual operations on the cursor and selections
 */

from StdIOCommon import ViewFrame, Rectangle, Point2
import EdMonad, EdMovement

vUpdateCursor	:: !Bool !Position !Int !FontInfo !Text !ViewFrame ![Rectangle]
												->	(*Picture -> *Picture)
// vUpdateCursor:	updates the cursor

vShowCursor		::									EditMonad (PSt *l) nothing
vHideCursor		::									EditMonad (PSt *l) nothing
// exported only for use by mouse functions to hide cursor during mouse edits

vCenterCursor	::									EditMonad (PSt *l) nothing
// vCenterCursor:	checks to see whether the cursor is within the view frame.
//					If it is not, the cursor is centered in the directions in which
//					it is necessary to make the cursor visible.

vScrollToCursor	::									EditMonad (PSt *l) nothing
// vScrollToCursor:	scrolls the view frame up to the point that the cursor 
//					becomes visible.

vMoveCursor		:: !Movement					->	EditMonad (PSt *l) nothing

vDoCursorSafe	:: (EditMonad (PSt *l) a)		->	EditMonad (PSt *l) a

vChangeSelectionTo	:: Selection				->	EditMonad (PSt *l) nothing
// vChangeSelectionTo:	changes the selection from the current selection
//						to the given selection and redraws, so that the display
//						reflects this change

//vUpdateSelection	:: ViewFrame [Rectangle]	->	EditMonad *Picture nothing
vUpdateSelection :: !Selection FontInfo Text ViewFrame [Rectangle]
												->	(*Picture -> *Picture)
// vUpdateSelection:	updates the selection in the frame 
//						within the given update area

vRemoveSelection	::								EditMonad (PSt *l) nothing
