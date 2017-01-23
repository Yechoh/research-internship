implementation module typewin

import StdString
import StdWindowDef
import StdPSt
//import IdeState

updateTypeWindow :: !Bool !String [Int] ![String] !*(PSt *b) -> *PSt *b | Typer b
updateTypeWindow _ _ _ _ ps = ps

tw_safe_close :: !*(PSt *b) -> *PSt *b | Typer b
tw_safe_close ps = ps

class Typer env
where
	getTypeWinInfo :: !*env -> *(!TypeWinInfo, !*env)
	setTypeWinInfo :: !TypeWinInfo !*env -> *env

:: TypeWinInfo = TWI

dummy_twi :: TypeWinInfo
dummy_twi = TWI

typeWinKeyboard :: Int
typeWinKeyboard = 42

typeWinMouse :: Int
typeWinMouse = 7
