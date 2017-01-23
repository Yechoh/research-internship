definition module EdActionType

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

import StdOverloaded
import EdMovement
from EdText import TextFragment

:: Action
	= Move		Movement
	| Insert	TextFragment
	| Scroll	Movement
	| Select	Movement
	| Remove	Movement

instance toString Action
instance fromString Action
instance == Action

allActions		:: [Action]
