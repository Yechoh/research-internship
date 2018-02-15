implementation module dcls

import iTasks
import shares
import System.OS
import extraText
import extraTaskCombinators
import qualified Data.Map as DM
import directoryBrowsing

readDcl :: [String] -> [(Sharenum,String,Comment)]
readDcl text
# text = filter (\line. line <> "") text
= readDcl` text
	where
		readDcl` [x:y]
		| isComment x
			# comment = join OS_NEWLINE [x:takeWhile (isComment) y]
			# [f:y] = dropWhile isComment y
			= [(Sharedf,f,comment):readDcl` y]
		| startsWith "import " x || startsWith "derive " x = [(Sharedi,x,""):readDcl` y]
		| contains "::" x = [(Sharedf,x,""): readDcl` y]
		| otherwise = readDcl` y

addDcl :: String -> Task ()
addDcl dclloc = readLinesFromFile dclloc >>- \mct ->
	case mct of
		Nothing = viewInformation "not found" [] dclloc >>| return ()
		(Just contenttxt) = get dclStore
			>>- \dcls ->
			set ('DM'.put dclloc (readDcl contenttxt) dcls) dclStore
			>>|- return ()

isComment :: String -> Bool
isComment a = startsWith "/*" a || startsWith "*" a || startsWith "//" a

readComment :: [String] -> Comment
readComment c
	= join OS_NEWLINE (map (\a.replaceSubString "*" "" (replaceSubString "*/" "" (replaceSubString "/*" "" a))) c)

updateDclStore :: Task (Map String [(Sharenum,String,Comment)])
updateDclStore =
	get dclStore >>- \dcls.
	get contents >>- \icls.
	set ('DM'.mapWithKey (\k v.newdcl k v.lines dcls) icls) dclStore

newdcl :: String [String] (Map String [(Sharenum,String,Comment)]) -> [(Sharenum,String,Comment)]
newdcl filename text dcls
# mdcl = 'DM'.get filename dcls
# dcl = case mdcl of Nothing = [] ; Just dcl = dcl
# sharable = filter (\a. contains "::" a || startsWith "import " a || startsWith "derive " a) text
# (indcl,inboth,inicl) = compare dcl sharable (\a b. snd3 a == b)
# indcl = map (\(x,y,z). (Deprecatedf,y,z)) (filter (\(x,y,z). z <> "") indcl) //if a description no longer has an implementation, throw it away, or make it deprecated if it has a comment. Imports cannot have a comment
# inboth = map (\(x,y,z). if (x === Deprecatedf) (Unsharedf,y,z) (x,y,z)) inboth //if a deprecated description has an implementation again, make it unshared
# inicl = map (\y. if (startsWith "import " y || startsWith "derive " y) (Unsharedi,y,"") (Unsharedf,y,"")) inicl //if a new implementation is made, make an unshared description.
= indcl ++ inboth ++ inicl

dclOf :: String -> Task [(Sharenum,String,Comment)]
dclOf filename = get dclStore >>- \dcls.
	case ('DM'.get filename dcls) of
		Nothing = viewInformation "not found" [] (filename,dcls) >>| return []
		Just content = return content
