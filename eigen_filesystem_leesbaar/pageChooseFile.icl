implementation module pageChooseFile

import iTasks
import pagetypes
import directoryBrowsing
import shares
import extraTaskCombinators
//import callCpm
import qualified Data.Map as DM
import extraText
import System.OS

pageChooseFile :: ChooseFileRedirects -> Task ()
pageChooseFile (actionContinue,pagenodeEditor) =
	askPath
	-||
	updateSharedInformation "paths" [] settings
	>>- \miclloc -> case miclloc of
		Nothing = pagenodeEditor
		(Just (a,b)) = setContents (a </> b) >>|- /*setProject iclloc >>|*/ (pagenodeEditor)

askPath :: Task (Maybe (String,String))
askPath
	=							getPwdName
	>>- \pwd ->					(selectFromTreeMaybe True pwd isCleanFile)
	//>>- \(a,b)->				return (Just (a </> b))
	//>>- \path -> return (takeDirectory path, dropDirectory path)

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

readComment :: [String] -> Comment
readComment c
	= join OS_NEWLINE (map (\a.replaceSubString "*" "" (replaceSubString "*/" "" (replaceSubString "/*" "" a))) c)



setContents :: String -> Task ()
setContents iclloc =
	readLinesFromFile (iclloc)>>- \mct ->
	case mct of
		Nothing = viewInformation "not found" [] iclloc >>| return ()
		(Just contenttxt) =	get contents
				>>- \contentmap ->
				set ('DM'.put iclloc contenttxt contentmap) contents
				>>|- return ()

//if there do not already exist	a temp icl and temp proj
	//create temp icl
	//if a proj already exist
		//base the temp proj on it
	//else
		//create new temp orj
/*setProject :: String String -> Task ()
setProject path name
	=							readFromFile (toproj (path </> name))
	>>- \mprojtxt ->			(case mprojtxt of
		Nothing ->					(cpmCreateProject (toproj name)
									>>|- return ())
		(Just projtxt) ->			return ()
								)
*/
