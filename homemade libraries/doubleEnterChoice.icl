implementation module doubleEnterChoice

import iTasks

doubleEnterChoiceWithShared :: d [ChoiceOption a] (ReadWriteShared [a] w) (a->d) (a->[ChoiceOption b]) (a->[b])-> Task a | toPrompt d & iTask a & iTask w & iTask b
doubleEnterChoiceWithShared d options container d2 options2 container2 =
	(enterChoiceWithShared d options container
	>&>
	second
	where
	second = (\sma. viewInformation "" [] "" ||- watch a)
	>>*
	[	OnValue (ifValue (\ma. isJust ma)) \(Just a).
		(second sma ||- enterChoiceWithShared (d2 a) (options2 a) (container2 a)) <<@ ApplyLayout (arrangeHorizontal)
	]) <<@ ApplyLayout (arrangeHorizontal)