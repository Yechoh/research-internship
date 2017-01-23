definition module typewin

import StdString
import StdWindowDef
import StdPSt
//import IdeState

updateTypeWindow :: !String [Int] ![String] !*(PSt *b) -> *PSt *b | Typer b

tw_safe_close :: !*(PSt *b) -> *PSt *b | Typer b

class Typer env
where
	getTypeWinInfo :: !*env -> *(!TypeWinInfo, !*env)
	setTypeWinInfo :: !TypeWinInfo !*env -> *env

:: TypeWinInfo

dummy_twi :: TypeWinInfo

typeWinKeyboard :: Int
typeWinMouse :: Int
