implementation module extraText

import StdOverloaded, StdString, StdArray, StdChar, StdInt, StdBool, StdClass, StdList
import Text

contains :: String String -> Bool
contains needle haystack = indexOf needle haystack <> -1

containsAfter :: Int String String -> Bool
containsAfter i needle haystack = indexOfAfter i needle haystack <> -1

splitOnce :: !String !String -> (String,String)
splitOnce sep s
	# index = indexOf sep s
	= (s%(0,index-1),s%(index+1,size s-1))

replaceFirstSubStringAt :: Int String String String -> String
replaceFirstSubStringAt i needle replacement haystack
	#! index = indexOfAfter i needle haystack
	| index == -1 = haystack
	| otherwise
		#! start = subString 0 index haystack
		#! end   = subString (index + size needle) (size haystack) haystack
		= start +++ replacement +++ end
