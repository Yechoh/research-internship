/*
 * EdTab.dcl: functions that deal with those annoying tab characters
 */

definition module EdTab

//1.3
from StdString	   import String
//3.1
from StdIOCommon   import Point2
from StdPicture	   import Picture
from StdPicture	   import Font, FontMetrics, FontName, FontSize, FontStyle, FontDef
from EdMonad       import FontInfo, TabSize, NewTabSize, SyntaxColours, Colour

splitAtTabs	   :: !String -> .[String]
// splitAtTabs:	   splits a string into several strings that were
//				   separated by tabs in the orginal string.
//				   e.g. "monkey\tcow\t" --> ["monkey","cow",""]

tabTake		   :: !Int [String] -> [String]
// tabTake:		   takes a number of characters of a string that
//				   is split at tabs. e.g. tabTake 4 ["abc","def"] =
//				   ["abc",""]

tabDrawString  :: !Point2 !String !FontInfo !*Picture -> *Picture
tabDrawStringC :: !Point2 !(!Int,!String) !FontInfo !*Picture -> *Picture
// tabDrawString:	draws a string containing tabs properly.
// tabDrawStringC:	draws a string containing tabs properly with syntax colours.

