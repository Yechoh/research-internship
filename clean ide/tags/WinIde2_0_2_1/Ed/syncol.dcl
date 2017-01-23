definition module syncol

// provides preparsing for Clean syntax colouring.

import StdString
import StrictList

:: Info	:== (!Int,!Bool)

firstParse :: !(StrictList String) -> StrictList (Info,String)
quickParse :: !Int !Int !(StrictList (Info,String)) -> (Int,StrictList (Info,String))
