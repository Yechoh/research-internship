definition module errorHandling

import iTasks
import shares

errorWindow :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()

build :: Task ()