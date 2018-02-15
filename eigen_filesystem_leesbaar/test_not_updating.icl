module test_not_updating

import iTasks
import iTasks.Extensions.Editors.Ace
import qualified Data.Map as DM
import dynamicTabs

Start world = startEngine editors world

instance zero AceState
	where
	zero = { lines             = []    //The lines of text in the editor
		, cursor            = (0,0) //The location of the cursor (<row>,<column>)
		, selection         = Nothing   //A text selection is delimited by this position and the cursor position
		, disabled          = False       //Disallow editing
		}

instance zero AceOptions
where
	zero = {
		theme = "",
		mode = ""
		}

content :: Shared (Map String AceState)
content = sharedStore "content" ('DM'.put "a" zero 'DM'.newMap)

editors :: Task [(Int,TaskValue ())]
editors = dynamicTabs content (id) (editor)

editor :: String (Shared AceState) -> Task ()
editor k content =
    (((updateSharedInformation "" [UpdateUsing (\c. (zero,c)) (\c (a,b) -> b) aceEditor] content
	>&^ \smc. viewSharedInformation "" [] smc) <<@ (ApplyLayout arrangeHorizontal))
	-|| viewSharedInformation "" [] content) >>| return ()
