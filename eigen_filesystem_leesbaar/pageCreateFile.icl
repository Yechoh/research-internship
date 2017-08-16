implementation module pageCreateFile

import iTasks
import qualified directoryBrowsing as DB
import extraTaskCombinators
import pagetypes
import shares
import qualified Data.Map as DM

pageCreateFile :: CreateFileRedirects -> Task ()
pageCreateFile ((actionCreate, pagenodeEditor),(actionCancel,pagenodeEditor2)) =
	(askFolder -&&- chooseFileName)
	>>*	[	OnAction actionCreate (hasValue(\(a,c).createFile (a,c) >>|- pagenodeEditor))
		,	OnAction actionCancel (always pagenodeEditor)
		]

askFolder :: Task String
askFolder
	=							'DB'.getPwdName
	>>= \pwd ->					updateInformation "Folder" [] pwd //('DB'.selectFolder pwd)

chooseFileName :: Task String
chooseFileName = enterInformation "Filename (without extension)" []

createFile :: (String,String) -> Task ()
createFile (a,b)
	 =
	'DB'.createFile a (b+++".icl") ("implementation module "+++b+++"\n\nimport iTasks\n\n") >>|-
	'DB'.createFile a (b+++".dcl") ("definition module "+++b+++"\n\nimport iTasks\n\n") >>|-
	setContents (a </> b+++".icl") >>|-
	upd (\dcls. 'DM'.put (a </> b+++".icl") [(Sharedi,"import iTasks","")] dcls) dclStore
	>>|- return ()

setContents :: String -> Task ()
setContents iclloc
	= 							'DB'.readLinesFromFile (iclloc)
	>>- \mct -> case mct of
		Nothing = viewInformation "" [] iclloc >>| return ()
		(Just contenttxt) =	get contents
				>>- \contentmap ->			set ('DM'.put iclloc contenttxt contentmap) contents
								>>|- return ()
