definition module set_return_code

from StdPSt import :: PSt

:: *UniqueWorld :== World

set_return_code_world	:: !Int !UniqueWorld -> UniqueWorld
set_return_code_pst		:: !Int    !(PSt .l) ->      PSt .l
