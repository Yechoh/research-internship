definition module clipboard

// Clipboard window handling

import StdId, StdPSt, StdMenuDef
import EdState

:: ClipInfo

class Clipper env where
	getClipInfo		:: !*env -> *(!ClipInfo,!*env)
	setClipInfo		:: !ClipInfo !*env -> *env

instance Clipper (PSt *p) | Clipper p

isClipboardWindow	:: !Id !*env -> (Bool,*env) | Clipper env

initClipInfo		:: Id [Id] *env -> *(ClipInfo,*env) | Ids env
clipMenuItems		:: !Id !Id !ClipInfo -> .MenuItem .c *(PSt *e) | Clipper , Editor e
cw_maybe_close		:: !Id !*(PSt *c) -> (Bool,*PSt *c) | Clipper, Editor c

// avoid compiler warnings...

:: ClipState`
instance == ClipState`
