implementation module shares

import iTasks
import qualified Data.Map as DM
import System.OS
import iTasks.Extensions.Editors.Ace
import System.FilePath
import extraText
import StdArray
import builddb
import CloogleDB
import extraList

derive class iTask Settings, Project, Sharenum

instance == Project
	where
		(==) p1 p2 = p1.projectName == p2.projectName

// ------------- global stores

settings :: Shared Settings
settings 	= sharedStore "settings" 	{ cleanHome	= ""
										}
project :: Shared Project
project 	= sharedStore "project" 	{ projectName		= ""
										, projectPath 		= ""
										, projectSources	= []
										}

dclStore :: Shared (Map String [(Sharenum,String,Comment)])
dclStore = sharedStore "dclstore" 'DM'.newMap

updateDclStore :: Task (Map String [(Sharenum,String,Comment)])
updateDclStore =
	get dclStore >>- \dcls.
	get contents >>- \icls.
	set ('DM'.mapWithKey (\k v.newdcl k v dcls) icls) dclStore

:: Sharenum = Sharedf | Unsharedf | Deprecatedf | Sharedi | Unsharedi
//:: Comment :== {description :: String, args :: [String], result :: String }
:: Comment :== String

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

searchterm :: Shared (String)
searchterm = sharedStore "searchterm" ""

clooglestore :: Shared (CloogleDB)
clooglestore = sharedStore "clooglestore" zero

errorStore :: Shared [String]
errorStore = sharedStore "errorstore" []

contents :: Shared (Map String [String])
contents = sharedStore "contents" 'DM'.newMap

contentLinesOf :: String -> Task [String]
contentLinesOf filepath =
	get contents >>- \c.
	case (fst ('DM'.getU filepath c )) of
		Nothing = viewInformation "not found" [] (filepath,c) >>| return ["not found!"]
		Just lines = return lines

setContent :: String [String] -> Task (Map String [String])
setContent filepath content = upd (\c. 'DM'.put filepath content c) contents

joinWithNewline :: String String -> String
joinWithNewline a b = a+++OS_NEWLINE+++b

remove_newlines :: String -> String
remove_newlines str = {c \\ c <-: str | c <> '\n' && c <> '\r'}

contentOf :: String -> Task String
contentOf filename = get contents >>- \c.
	return (foldr (\a. joinWithNewline (remove_newlines a)) "" (fromJust (fst ('DM'.getU filename c ))))

/*contents uses the complete filepath as filename.
This is because a user is allowed to have two files open with the same name but on different locations.
But sometimes we only know the filename.
This function finds the corresponding filepath*/
filenameToFilepath :: String -> Task (Maybe String)
filenameToFilepath filename =
	get contents >>- \c.
	case ('DM'.mapSize ('DM'.filterWithKey (\k a. takeFileName k == filename) c)) of
		1 = return (Just (hd ('DM'.keys ('DM'.filterWithKey (\k a. takeFileName k == filename) c))))
		0 = return Nothing
		a = enterChoice "Multiple files found with this name. Choose one:" [] ('DM'.keys ('DM'.filterWithKey (\k a. takeFileName k == filename) c))
			>>* [	OnAction ActionCancel (always (return Nothing))
				,	OnAction (Action "Choose") (hasValue \name. return (Just name))
				]

:: Shortcut = No_shortcut
			| Ctrl_slash
			| Ctrl_backslash
			| Ctrl_Shift_backslash
			| Ctrl_equals
			| Ctrl_Shift_equals
			| Ctrl_b //
			| Ctrl_d
			| Ctrl_Shift_d
			| Ctrl_e
			| Ctrl_Shift_e
			| Ctrl_i
			| Ctrl_l
			| Ctrl_m
			| Ctrl_n
			| Ctrl_o
			| Ctrl_r
			| Ctrl_Shift_r
			| Ctrl_s
			| Ctrl_Shift_s
			| Ctrl_Alt_s
			| Ctrl_Shift_Alt_s
			| Ctrl_w
			| Ctrl_Shift_w

:: EditorInfo =
	{
		shortcuts :: [Shortcut],
		selection :: Maybe AceRange,
		position :: (Int,Int),
		theme :: String,
		readOnly :: Bool,
		prev_time :: Time
	}

derive class iTask EditorInfo, Shortcut
