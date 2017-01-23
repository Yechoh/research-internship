definition module errwin

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

// errors and warnings window

import StdPSt, StdId
import IdeState

sr_find_err			:: !Bool !*(PSt *General) -> *PSt *General
// Goto next error in error window

updateErrorWindow :: !([String]) !*(PSt *General) -> *PSt *General;
// Add lines to error window. If not open then open it

ew_safe_close			:: !*(PSt *General) -> *PSt *General
ew_maybe_close			:: !Id !*(PSt *General) -> (Bool,*PSt *General)

err_init			:: !ErrPrefs *World -> *(.ErrorInfo .c,*World)
// Initialise options from prefs

err_shut			:: !(ErrorInfo .a) -> !ErrPrefs
// Initialise prefs from options

err_options			:: !*(PSt *General) -> *PSt *General;
// Show options dialog for error window
