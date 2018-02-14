implementation module doubleEnterChoice

import iTasks
import extraTaskCombinators

/*
doubleEnterChoiceWithShared :: d [ChoiceOption a] (ReadWriteShared [a] w) (a->d) (a->[ChoiceOption b]) (a->[b])-> Task b | toPrompt d & iTask a & iTask w & iTask b
doubleEnterChoiceWithShared d options container d2 options2 container2 =
	(enterChoiceWithShared d options container
	>&>
	second) <<@ ApplyLayout (arrangeHorizontal)
	where
	second = (\sma. viewInformation "" [] "" ||- watch sma
		>>*
		[	(OnValue (ifValue (\ma. isJust ma) \(Just a).
			second`) )
		])
	second` =
		enterChoice (d2 a) (options2 a) (container2 a) >&> watch sma
*/

chooseTaskBasedOnElementOfSharedList :: d (Shared [a]) (a->d2) (a->[a2]) (a a2 ->Task ()) -> Task () | toPrompt d & iTask a & toPrompt d2 & iTask a2 & ==a
chooseTaskBasedOnElementOfSharedList d container d2 container2 taskFun =
	first >>|- return ()
	where
	first =
		(enterChoiceWithShared d [ChooseFromGrid id] container
		>&^
		(\sma. viewInformation "" [] "" ||- watch sma
			>>*
			[	(OnValue (ifValue (\ma. isJust ma) \(Just a).
				second sma a d2 container2 taskFun) )
			])) <<@ ApplyLayout (arrangeHorizontal)

/*chooseElementBasedOnFeed :: (Task a) (a->d2) (a->[a2]) (a a2 ->b) -> Task () | toPrompt d & iTask a & toPrompt d2 & iTask b & iTask a2 & ==a
chooseElementBasedOnFeed containertask d2 container2*/

chooseTaskBasedOnFeed :: (Task a) (a->d2) (a->[a2]) (a a2 ->Task ()) -> Task () | iTask a & toPrompt d2 & iTask a2
chooseTaskBasedOnFeed containertask d2 container2 taskFun =
	first >>|- return ()
	where
	first =
		(containertask
		>&^
		(\sma. viewInformation "" [] "" ||- watch sma
			>>*
			[	(OnValue (ifValue (\ma. isJust ma) \(Just a).
				second sma a d2 container2 taskFun) )
			])) <<@ ApplyLayout (arrangeHorizontal)

chooseTaskBasedOnFeed2 :: (Task a) (a->d2) (a->Task [a2]) (a2->d3) (a a2 ->Task ()) -> Task () | iTask d3 & iTask a & toPrompt d2 & iTask a2
chooseTaskBasedOnFeed2 containertask d2 container2 d3 taskFun =
	first >>|- return ()
	where
	first =
		(containertask
		>&^
		(\sma. viewInformation "" [] "" ||- watch sma
			>>*
			[	(OnValue (ifValue (\ma. isJust ma) \(Just a).
				second2 sma a d2 container2 d3 taskFun) )
			])) <<@ ApplyLayout (arrangeHorizontal)

second2 :: (ReadOnlyShared (Maybe a)) a (a->d2) (a->Task [a2]) (a2->d3) (a a2 -> Task ()) -> Task () | iTask a & toPrompt d2 & iTask a2 & iTask d3
second2 sma a d2 container2 d3 taskFun = container2 a >>- \c2.
		second2` c2 sma a d2 container2 d3 taskFun

second2` :: [a2] (ReadOnlyShared (Maybe a)) a (a->d2) (a->Task [a2]) (a2->d3) (a a2 -> Task ()) -> Task () | iTask a & toPrompt d2 & iTask a2 & iTask d3
second2` c2 sma a d2 container2 d3 taskFun =
	(((enterChoice (d2 a) [ChooseFromGrid d3] (c2))
	>&>
	\sma2. watch (sma >*< sma2))
		>>*
		[ (OnValue (ifValue (\(ma,ma2). isJust ma2) \(ma,Just a2).
		(second2 sma a d2 container2 d3 taskFun -||- taskFun a a2)))
		, (OnValue (ifValue (\(ma,ma2). maybe (False) (\newa. newa =!= a) ma) (\(Just newa,ma2).
		second2 sma newa d2 container2 d3 taskFun)))
		])

second :: (ReadOnlyShared (Maybe a)) a (a->d2) (a->[a2]) (a a2 -> Task ()) -> Task () | iTask a & toPrompt d2 & iTask a2
second sma a d2 container2 taskFun =
		(enterChoice (d2 a) [ChooseFromGrid id] (container2 a)
		>&>
		\sma2. watch (sma >*< sma2))
			>>*
			[ (OnValue (ifValue (\(ma,ma2). isJust ma2) \(ma,Just a2).
			(second sma a d2 container2 taskFun -||- taskFun a a2)))
			, (OnValue (ifValue (\(ma,ma2). maybe (False) (\newa. newa =!= a) ma) (\(Just newa,ma2).
			second sma newa d2 container2 taskFun)))
			]
