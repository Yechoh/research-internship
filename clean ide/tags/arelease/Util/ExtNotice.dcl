definition module ExtNotice

/*
0.0 [P88] The original notice class from the Object IO tutorial
1.0 [DvA] Modified for use with the new Clean IDE
2.0 [DvA] Added TimedNotice class

P88 = Peter Achten (peter88@cs.kun.nl)
DvA = Diederik van Arkel (diederik@cs.kun.nl)
*/

import StdWindow, StdTimerDef

:: Notice ls ps
	= Notice [String] (NoticeButton *(ls,ps)) [NoticeButton *(ls,ps)]

:: NoticeButton ps
	= NoticeButton String (IdFun ps)

instance Dialogs Notice

openNotice :: !(Notice .ls *(PSt .l)) !*(PSt .l) -> *PSt .l

okNotice text ps
	:== openNotice (Notice text (NoticeButton "OK" (\x->x)) []) ps

:: TimedNotice ls ps
	= TimedNotice [String] TimerInterval (NoticeButton *(ls,ps)) [NoticeButton *(ls,ps)]

instance Dialogs TimedNotice

openTimedNotice :: !(TimedNotice .ls *(PSt .l)) !*(PSt .l) -> *PSt .l

okTimedNotice text time ps
	:== openTimedNotice (TimedNotice text time (NoticeButton "OK" (\x->x)) []) ps
