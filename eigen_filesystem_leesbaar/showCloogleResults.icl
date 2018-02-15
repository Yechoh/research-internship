implementation module showCloogleResults

import iTasks
import extraTaskCombinators
import fileTraveling
import shares

showCloogleResults :: String -> Task ()
showCloogleResults a = get clooglestore >>- \db.
	((enterChoice "" [ChooseFromGrid \result. (snd result).fe_representation] (findFunction a db))
	>>*
	[OnValue (hasValue \(loc,fe). openLocation loc ||- showCloogleResults a)]
	>>|- return ())
