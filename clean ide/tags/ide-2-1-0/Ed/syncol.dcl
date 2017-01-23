definition module syncol

// provides preparsing for Clean syntax colouring.

import StdString
import StrictList

:: Info	:== 
	(!Int		// comment nesting level at start of line
//	,!Bool		// in typedef at start of line
	,!Bool		// is typedef line
//	,!Bool		// in typedecl at start of line
	,!Bool		// is typedecl line
	,!Int		// context offside level
	,!Bool		// flush accu
	)
// pack bools into bitfield?

firstParse :: !(StrictList String) -> StrictList (!Info,!String)
quickParse :: !Int !Int !(StrictList (!Info,!String)) -> (Int,Int,StrictList (!Info,!String))
