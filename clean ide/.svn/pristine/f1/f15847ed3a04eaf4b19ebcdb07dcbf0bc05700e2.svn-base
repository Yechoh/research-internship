definition module typewin

import StdPSt, StdId, StdWindowDef
from UtilStrictLists import List
from EdMonad import EditState, SyntaxColours, EditMonad, StateM
from PmPrefs import TypPrefs

//--- Types window

iniTypeWinInfo :: !Id !Id ![Id] !TypPrefs !*a -> *(.TypeWinInfo,*a) | accScreenPicture, Ids a
// initialise type window info

updateTypeWindow :: !String [WindowAttribute *(EditState,*(PSt *b))] ![String] !*(PSt *b) -> *PSt *b | Typer b
// add content to the types window

isTypeWindow :: !Id !.TypeWinInfo -> Bool
// is window the Types window

tw_maybe_close :: !Id !*(PSt *b) -> *(Bool,*(PSt *b)) | Typer b
// tw_maybe_close :: close window if it is types window

tw_safe_close :: !*(PSt *b) -> *PSt *b | Typer b
// tw_safe_close :: close types window

//-

from EdMessage import EditId

class Typer env
where
	getTypeWinInfo :: !*env -> *(!TypeWinInfo, !*env)
	setTypeWinInfo :: !TypeWinInfo !*env -> *env

:: TypeWinInfo

// TypeWinInfo accessors
TW_GetInf :: !TypeWinInfo -> TypPrefs
TW_SetUpd :: ![Id] !TypeWinInfo -> TypeWinInfo

// message passing with types window
maybe_type_win_message :: !Id .(EditMonad *(PSt *b) .c) !*(PSt *b) -> (Maybe .c,*(PSt *b)) | Typer b
type_win_message :: !.(EditMonad *(PSt *b) .c) !*(PSt *b) -> *(Maybe .c,*(PSt *b)) | Typer b

// dialogue to set colours for types window
typewinColours :: !*(PSt *a) -> *(PSt *a) | Typer a
