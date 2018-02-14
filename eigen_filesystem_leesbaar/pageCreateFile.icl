implementation module pageCreateFile

import iTasks
import qualified directoryBrowsing as DB
import extraTaskCombinators
import pagetypes
import shares
import qualified Data.Map as DM
import pageEditor
import content

pageCreateFile :: Task ()
pageCreateFile =
	(askFolder -&&- chooseFileName)
	>>*	[	OnAction (Action "Create") (hasValue(\(a,c).createFile (a,c) >>|- pageEditor))
		,	OnAction (Action "Cancel") (always pageEditor)
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
