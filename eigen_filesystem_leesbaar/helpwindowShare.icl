implementation module helpwindowShare

import iTasks
import extraText
import extraTaskCombinators
import qualified Data.Map as DM
import shares
import iTasks.Extensions.Editors.Ace
import iTasks.UI.Editor.Builtin


helpwindowShare :: String -> Task ()
helpwindowShare filename = (get dclStore >>- \dcls. maybe (show []) (show) ('DM'.get filename dcls) >>* [OnAction (Action "Refresh") (always (helpwindowShare filename))]) >>|- return ()
where
	show dcl
	# imports = filter (\(x,y,z). startsWith "import " y || startsWith "derive " y) dcl
	# functions = filter (\(x,y,z). not (startsWith "import " y || startsWith "derive " y)) dcl
	= functiontasks (importtasks (headline) imports) functions
	where
		importtasks starttask imports = (foldr (\d t. t ||- showSharableImport filename d) (starttask) imports)
		functiontasks starttask functions = (foldr (\d t. t ||- showSharableFunction filename d) (starttask) functions)
		headline = ((((viewInformation "" [] "Shared:" -&&- viewInformation "" [] "Definition:")<<@ ApplyLayout (arrangeHorizontal)) -&&- viewInformation "" [] "Description:")<<@ ApplyLayout (arrangeHorizontal)) >>|- return ()

swapshared :: Sharenum -> Sharenum
swapshared Sharedi = Unsharedi
swapshared Unsharedi = Sharedi
swapshared Deprecatedf = Deprecatedf //should never occur
swapshared Sharedf = Unsharedf
swapshared Unsharedf = Sharedf

/*
showSharable :: [(Sharenum,String,Comment)]
showSharable filename dcl =
 	enterChoice "Shared" [ChooseFromGrid (\dcl. fst3 dcl === Sharedi || fst3 dcl === Sharedf)] dcl >>* [OnValue (hasValue (\(x,y,z). upd (\dcls. 'DM'.put filename (\dcl. map (\(x2,y2,z2). if (y2 == y) (swapshared x,y2,z2) (x2,y2,z2) ) dcl) dcls) dclsStore))]
	-&&-
	enterChoice "Definition" [ChooseFromGrid (snd3)] dcl
	-&&-
	enterChoice "Description"
*/

showSharableImport :: String (Sharenum,String,Comment) -> Task ()
showSharableImport filename (sharenum,def,desc) =
 	(((viewInformation "" [] (sharenum === Sharedi)
	-&&-
	viewInformation "" [] def)<<@ ApplyLayout (arrangeHorizontal))
	-&&-
	viewInformation "" [] "")<<@ ApplyLayout (arrangeHorizontal)
	>>*
	[ OnAction (Action (if (sharenum === Sharedi) "Unshare" "Share"))  (hasValue (\((x,y),z). upd (\dcls.('DM'.alter (\(Just dcl). Just (swapmap y dcl)) filename dcls)) dclStore ||- showSharableImport filename (swapshared sharenum,y,z)))
	]
	where
		swapmap :: String -> ([(Sharenum,String,Comment)] -> [(Sharenum,String,Comment)])
		swapmap y = (\dcl. map (\(x2,y2,z2). if (y2 == y) (swapshared sharenum,y2,z2) (x2,y2,z2) ) dcl)

showSharableFunction :: String (Sharenum,String,Comment) -> Task ()
showSharableFunction filename (sharenum,def,desc) =
 	(((viewInformation "" [] (sharenum === Sharedf)
	-&&-
	viewInformation "" [] def)<<@ ApplyLayout (arrangeHorizontal))
	-&&-
	updateInformation "" [UpdateUsing id (\a b. b) (textArea 'DM'.newMap)] desc)<<@ ApplyLayout (arrangeHorizontal)
	>>*
	[ OnAction (Action (if (sharenum === Sharedf) "Unshare" "Share"))  (hasValue (\((x,y),z). upd (\dcls.('DM'.alter (\(Just dcl). Just (swapmap y dcl)) filename dcls)) dclStore ||- showSharableFunction filename (swapshared sharenum,y,z)))
	, OnAction (Action "Save description") (hasValue \((x,y),z). upd (\dcls. ('DM'.alter (\(Just dcl). Just (commentmap y z dcl)) filename dcls)) dclStore ||- showSharableFunction filename (sharenum,y,z))
	]
	where
		swapmap :: String -> ([(Sharenum,String,Comment)] -> [(Sharenum,String,Comment)])
		swapmap y = (\dcl. map (\(x2,y2,z2). if (y2 == y) (swapshared sharenum,y2,z2) (x2,y2,z2) ) dcl)

		commentmap :: String Comment -> ([(Sharenum,String,Comment)] -> [(Sharenum,String,Comment)])
		commentmap y z = (\dcl. map (\(x2,y2,z2). if (y2 == y) (x2,y2,z) (x2,y2,z2) ) dcl)
