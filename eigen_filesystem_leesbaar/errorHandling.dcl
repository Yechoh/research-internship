definition module errorHandling

import iTasks
import shares

errorWindow :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()

placeText :: String Int [String] -> Task ()

jumpToLine :: (Shared (EditorInfo,Map String [String])) Int -> Task ()

build :: Task ()
