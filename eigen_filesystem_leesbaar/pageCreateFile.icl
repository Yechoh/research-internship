implementation module pageCreateFile

import iTasks
import qualified directoryBrowsing as DB
import extraTaskCombinators
import pagetypes
import shares
import qualified Data.Map as DM

pageCreateFile :: CreateFileRedirects -> Task ()
pageCreateFile ((actionCreate, pagenodeEditor),(actionCancel,pagenodeEditor2)) = 
	askFolder -&&- chooseFileName
	>>*	[	OnAction actionCreate (hasValue(\((a,b),c).createFile ((a,b),c) >>|- pagenodeEditor))
		,	OnAction actionCancel (always pagenodeEditor)
		]
	
askFolder :: Task ((String,String))
askFolder
	=							'DB'.getPwdName
	>>- \pwd ->					('DB'.selectFolder pwd)
	
chooseFileName :: Task String
chooseFileName = enterInformation "filename (without extension)" [] 

createFile :: ((String,String),String) -> Task ()
createFile ((aa,ba),ca)
	# a = aa </> ba
	# b = ca
	 = 
	'DB'.createFile a (b+++".icl") ("implementation module "+++b+++"\n\nimport iTasks\n\n") >>|-
	'DB'.createFile a (b+++".dcl") ("definition module "+++b+++"\n\nimport iTasks\n\n") >>|-
	setContents (a </> b+++".icl") >>|-
	setContents (a </> b+++".dcl")
setContents :: String -> Task ()
setContents iclloc
	= 							'DB'.readLinesFromFile (iclloc)
	>>- \mct -> case mct of
		Nothing = viewInformation "" [] iclloc >>| return ()
		(Just contenttxt) =	get contents
				>>- \contentmap ->			set ('DM'.put iclloc contenttxt contentmap) contents
								>>|- return ()	