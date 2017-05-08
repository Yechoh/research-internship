definition module ideUtil

import iTasks

(>>|-) infixl 1 :: (Task a) (Task b) -> Task b | iTask a & iTask b

mbCancel :: (Task a) -> Task ()

// obscure try-outs 

showOrHide  	 :: !String !Bool !(Task ()) -> Task ()
optionallyHidden :: !String !Bool !(Task a) -> Task a | iTask a
showOrHide2 ::  !String !Bool !(Task a) -> Task a | iTask a

evalMyTask :: HTTPRequest -> Task ()				// not working yet

