definition module EdMouse

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
 * EdMouse.dcl: handling mouse events
 */

from StdIOCommon import MouseState, Point2, Modifiers
from StdPSt		 import PSt, IOSt
import EdMonad, EdCommon

editWindowMouse :: MouseState (!EditState, !PSt PLocState) -> (EditState, PSt PLocState)
// editWindowMouse: handles mouse events in the edit window

noMouseMoved :: !MouseState -> Bool
