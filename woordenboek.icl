module woordenboek

import iTasks

Start world = startEngine (woordenboek woorden) world

:: WoordStructure = Wcons [String] WoordStructure | Wnil

woorden :: WoordStructure
woorden = Wcons ["appel", "aardappel", "aanhanger"] (Wcons ["banaan", "beer", "boef"] (Wcons ["computer", "cactus", "cavia"] Wnil))

woordenboeksimpel :: WoordStructure -> Task String
woordenboeksimpel (Wcons l ws) = enterChoice "" [(ChooseFromList (\x.x))] l 
woordenboeksimpel Wnil = return ""

woordenboek :: WoordStructure -> Task String
woordenboek (Wcons l ws) = enterChoice "" [(ChooseFromList (\x.x))] l 
	>>* [ 	OnAction (Action "next page") (always(woordenboek ws))
		,	OnAction ActionContinue (hasValue (\x.return x))
		]
woordenboek Wnil = return ""