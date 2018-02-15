implementation module content

import iTasks
import extraTaskCombinators
import shares
import directoryBrowsing
import qualified Data.Map as DM
import System.OS

setContents :: String -> Task ()
setContents iclloc =
	readLinesFromFile (iclloc)>>- \mct ->
	case mct of
		Nothing = viewInformation "not found" [] iclloc >>| return ()
		(Just contenttxt) =	get contents
				>>- \contentmap ->
				setContent iclloc {zero & lines=contenttxt}
				>>|- return ()

scontentOf :: String -> Shared AceState
scontentOf filename = mapReadWrite  (getc filename,putc filename) contents

getc :: String (Map String AceState) -> AceState
getc filename contents = maybe zero id ('DM'.get filename contents)

putc :: String AceState (Map String AceState) -> Maybe (Map String AceState)
putc filename content contents = Just ('DM'.put filename content contents)

contentOf :: String -> Task AceState
contentOf filepath =
	get contents >>- \c.
	case ('DM'.get filepath c ) of
		Nothing = viewInformation "not found" [] (filepath,c) >>| return {zero & lines=["not found!"]}
		Just content = return content

updateContentText :: String ([String]->[String]) -> Task (Map String AceState)
updateContentText filepath textf =
	contentOf filepath >>- \content.
	upd (\c. 'DM'.put filepath {content & lines = textf content.lines} c) contents

setContentText :: String [String] -> Task (Map String AceState)
setContentText filepath text =
	contentOf filepath >>- \content.
	upd (\c. 'DM'.put filepath {content & lines = text} c) contents

setContent :: String AceState  -> Task (Map String AceState)
setContent filepath content =
	upd (\c. 'DM'.put filepath content c) contents

updateContent :: String (AceState -> AceState) -> Task (Map String AceState)
updateContent filepath contentf =
	contentOf filepath >>- \content.
	upd (\c. 'DM'.put filepath (contentf content) c) contents

//returns
contentStringOf :: String -> Task String
contentStringOf filepath = get contents >>- \c.
	case ('DM'.get filepath c) of
		Nothing = viewInformation "not found" [] (filepath,c) >>| return "not found!"
		Just (content) = return (foldr (\a. joinWithNewline (remove_newlines a)) "" content.lines)

/*contents uses the complete filepath as filename.
This is because a user is allowed to have two files open with the same name but on different locations.
But sometimes we only know the filename.
This function finds the corresponding filepath in the open contents*/
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

placeText :: String Int [String] -> Task ()
placeText filepath i text =
	updateContentText filepath (\l.(\(a,b).a++text++b) (splitAt i l)) >>|- return ()

jumpToLine :: String Int -> Task ()
jumpToLine filename i =
	updateContent filename (\content.{content & cursor = (i, 0)}) >>|- return ()

joinWithNewline :: String String -> String
joinWithNewline a b = a+++OS_NEWLINE+++b

remove_newlines :: String -> String
remove_newlines str = {c \\ c <-: str | c <> '\n' && c <> '\r'}
