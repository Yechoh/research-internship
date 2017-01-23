definition module flexwin

import StdEnv, StdIO
//import ShowProfile

//::	FlexBarWindow ls pst = FlexBarWindow Title [(String, Maybe Int)] [WindowAttribute *(ls,pst)]
//::	FlexBarWindow ls pst
//	= FlexBarWindow Title [(String, Maybe Int)] .[FormattedProfile] (R2Id (MessageIn ls) MessageOut) [WindowAttribute *(ls,pst)]
//::	FlexBarWindow s ls pst
//	= FlexBarWindow Title [(String, Maybe Int)] [s] (R2Id (MessageIn s) MessageOut) [WindowAttribute *(ls,pst)]

class content_size c :: FontMetrics c -> Int

::FlexBarState s
::	FlexBarWindow s ls pst
	= FlexBarWindow Title [(String, Maybe Int)] s
		(!s .Int .Int [.Int] -> (.SelectState .UpdateState -> .(*Picture -> *Picture)))
		![(FlexBarState s) -> FlexBarState s]
		(R2Id (MessageIn s) (MessageOut s)) [WindowAttribute *(ls,pst)]

:: MessageIn s
	= FW_DummyIn
	| FW_SetContent s		//[.FormattedProfile]
	| FW_ApplyFunction Int
	| FW_GetContent

:: MessageOut s
	= FW_DummyOut
	| FW_ContentOut s

instance Windows (FlexBarWindow s) | content_size s

//--

appInfo :: (s->s) !(FlexBarState s) -> FlexBarState s
