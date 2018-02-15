definition module content

import iTasks
import shares

placeText :: String Int [String] -> Task ()

jumpToLine :: String Int -> Task ()

setContents :: String -> Task ()

//functions to get specific content
scontentOf :: String -> Shared AceState
contentOf :: String -> Task AceState
updateContentText :: String ([String]->[String]) -> Task (Map String AceState)
setContentText :: String [String] -> Task (Map String AceState)
setContent :: String AceState  -> Task (Map String AceState)
updateContent :: String (AceState -> AceState) -> Task (Map String AceState)
joinWithNewline :: String String -> String
contentStringOf :: String -> Task String
filenameToFilepath :: String -> Task (Maybe String)
