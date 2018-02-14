module pagesWebIDE

//import iTasks
import pageChooseFile
import pageEditor
import pageAskImportPaths
import qualified Data.Map as DM
import shares
import settings
import pageCreateFile
import errorHandling

Start world =
	startEngine
	(startTask)
	world

// first force settings after which by default we open an editor on the main .icl file
startTask :: Task ()
startTask
	=				get settings
	>>- \curSet ->	get project
	>>- \curProj ->	let isProj = curProj.projectPath <> "" && curProj.projectName <> ""
					    isCPM  = curSet.cleanHome <> ""
					in if (not isProj || not isCPM)
							(setSettings  >>| startTask)
							(get contents >>- \c.
							if ('DM'.mapSize c == 0)
							pageChooseFile
							pageEditor)
