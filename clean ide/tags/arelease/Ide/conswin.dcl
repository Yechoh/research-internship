definition module conswin

// The 'Console' window...

import StdPSt, StdId, StdWindowDef
from EdMonad import EditState, SyntaxColours, EditMonad, StateM
from PmPrefs import ConPrefs

:: ConsWinInfo

class Consoler env		// Class for embedding console window information in a larger state
where
	getConsWinInfo		:: !*env -> *(!ConsWinInfo, !*env)
	setConsWinInfo		:: !ConsWinInfo !*env -> *env

isConsoleWindow			:: !Id !.ConsWinInfo -> Bool
// is window with id the console window?

iniConsWinInfo ::		// initialise console window information
	!Id					// edit menu id
	!Id					// undo item id
	![Id]				// ids to disable when console is active
	!ConPrefs			// console window preferences
	!*a -> *(.ConsWinInfo,*a) | accScreenPicture, Ids a

updateConsoleWindowI	:: !String [WindowAttribute *(EditState,*(PSt *b))] !*(PSt *b) -> *PSt *b | Consoler b
// update console window with stdin info
updateConsoleWindowO	:: !String [WindowAttribute *(EditState,*(PSt *b))] !*(PSt *b) -> *PSt *b | Consoler b
// update console window with stdout info
updateConsoleWindowE	:: !String [WindowAttribute *(EditState,*(PSt *b))] !*(PSt *b) -> *PSt *b | Consoler b
// update console window with stderr info
maybe_cons_win_message2	:: .(EditMonad *(PSt *b) .c) !*(PSt *b) -> (Maybe .c,*(PSt *b)) | Consoler b
// send message to console window

getConPrefs				:: !ConsWinInfo -> ConPrefs

conswinColours			:: !*(PSt *a) -> *(PSt *a) | Consoler a
// present a dialogue for setting console window colours
