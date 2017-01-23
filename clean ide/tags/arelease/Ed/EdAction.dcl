definition module EdAction

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
 * EdAction.dcl: performing editor Actions
 */

from EdSelection	import Position, ColumnNr, LineNr
from EdText			import TextFragment, StrictList

//1.3
from EdText import String
//3.1

import EdMonad, EdMovement
import EdCommon
from EdActionType	import Action

performAction	:: Action -> EditMonad (PSt PLocState) nothing

undoAction :: EditMonad (PSt *l) nothing
