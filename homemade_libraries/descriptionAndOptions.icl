implementation module descriptionAndOptions

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

viewSharedListWithDescriptionAndOptions :: (Shared [a]) (a -> c) (a -> String) (a -> [(String,Task b)]) -> Task b | iTask a & iTask b & iTask c & == c
viewSharedListWithDescriptionAndOptions a elToRepresentation elToDescription elToOptions = 
	(enterChoiceWithShared "" [ChooseFromGrid elToRepresentation] a
	>&>
	(\a.(sharedDescriptionAndOptions (a) (elToRepresentation) (elToDescription) (elToOptions)))) <<@ ApplyLayout (arrangeHorizontal)
	
sharedDescriptionAndOptions :: (ReadOnlyShared (Maybe a)) (a->c) (a -> String) (a -> [(String,Task b)])  -> Task b | iTask a & iTask b & iTask c & == c
sharedDescriptionAndOptions sma aToRepresentation aToDescription aToOptions =
	(viewInformation "" [] "" ||- watch sma)
	>>*
	[	OnValue (ifValue (\ma. isJust ma) (\(Just a). 
		showSharedButtons 
			aToRepresentation 
			(aToDescription a) 
			(aToOptions a) 
			(aToRepresentation a) 
			sma 
			(sharedDescriptionAndOptions sma aToRepresentation aToDescription aToOptions)))
	]

showSharedButtons :: (a->c) String [(String,Task b)] c (ReadOnlyShared (Maybe a)) (Task b) -> Task b | iTask a & iTask b & iTask c & == c
showSharedButtons aToRepresentation description options representation sma parentTask = 
	viewInformation "" [] description ||- watch sma
	>>*
	[	OnValue (ifValue (\ma. (maybe (False) (\a. (aToRepresentation a) <> representation) ma)) \a.parentTask)
	:	(map (\(a,t). OnAction (Action a) (always t)) options)
	]
	
viewListWithDescriptionAndOptions :: [c] [String] [[(String,Task b)]] -> Task b | iTask b & iTask c
viewListWithDescriptionAndOptions representations descriptions optionss =
	(enterChoice "" [ChooseFromGrid (\i.representations!!i)] [0..((length representations)-1)]
	>&>
	(\smi. descriptionAndOptions smi descriptions optionss)) <<@ ApplyLayout (arrangeHorizontal)

descriptionAndOptions :: (ReadOnlyShared (Maybe Int)) [String] [[(String,Task b)]] -> Task b | iTask b
descriptionAndOptions smi descriptions optionss =
	(viewInformation "" [] "" ||- watch smi)
	>>*
	[	OnValue (ifValue (\mi.isJust mi) \(Just i).
		showButtons
			smi
			i
			descriptions
			optionss
			(descriptionAndOptions smi descriptions optionss)
		)
	]
	
showButtons :: (ReadOnlyShared (Maybe Int)) Int [String] [[(String,Task b)]] (Task b) -> Task b | iTask b
showButtons smi i descriptions optionss parentTask =
	viewInformation "" [] (descriptions!!i) ||- watch smi
	>>*
	[	OnValue (ifValue (\ma. (maybe (False) (\a. a <> i) ma)) \a.parentTask)
	:	(map (\(a,t). OnAction (Action a) (always t)) (optionss!!i))
	]

viewSharedListWithDescriptionAndOptionsSideways :: (Shared [a]) (a -> c) (a -> String) (a -> [(String,Task b)]) -> Task b | iTask a & iTask b & iTask c & == c
viewSharedListWithDescriptionAndOptionsSideways a elToRepresentation elToDescription elToOptions = 
	(enterChoiceWithShared "" [ChooseFromGrid elToRepresentation] a
	>&>
	(\a.(sharedDescriptionAndOptions (a) (elToRepresentation) (elToDescription) (elToOptions)))) <<@ ApplyLayout (arrangeHorizontal)
	
sharedDescriptionAndOptions :: (ReadOnlyShared (Maybe a)) (a->c) (a -> String) (a -> [(String,Task b)])  -> Task b | iTask a & iTask b & iTask c & == c
sharedDescriptionAndOptions sma aToRepresentation aToDescription aToOptions =
	(viewInformation "" [] "" ||- watch sma)
	>>*
	[	OnValue (ifValue (\ma. isJust ma) (\(Just a). 
		showSharedButtons 
			aToRepresentation 
			(aToDescription a) 
			(aToOptions a) 
			(aToRepresentation a) 
			sma 
			(sharedDescriptionAndOptions sma aToRepresentation aToDescription aToOptions)))
	]

showSharedButtons :: (a->c) String [(String,Task b)] c (ReadOnlyShared (Maybe a)) (Task b) -> Task b | iTask a & iTask b & iTask c & == c
showSharedButtons aToRepresentation description options representation sma parentTask = 
	viewInformation "" [] description ||- watch sma
	>>*
	[	OnValue (ifValue (\ma. (maybe (False) (\a. (aToRepresentation a) <> representation) ma)) \a.parentTask)
	:	(map (\(a,t). OnAction (Action a) (always t)) options)
	]
/*	
showButtons :: String [(String,Task ())] String (ReadOnlyShared (Maybe String)) (Task ()) -> Task ()
showButtons message l str smstr t = viewInformation "" [] message ||- watch smstr >>*
		[(OnValue (ifValue (\mstr. mstr <> (Just str)) \a.t)):(map (\(a,t). OnAction a (always t)) l)]

errorWindow :: String (Shared (EditorInfo,Map String [String])) -> ParallelTask ()
errorWindow nameOfFileInEditor editorStore
	=				\tasklist.(get project
	>>- \myProj ->	accWorld (fileExists (myProj.projectPath </> myProj.projectName +++ ".exe"))
	>>- \isExe -> get contents 
	>>- \c -> 'DM'.foldrWithKey (\k v t -> t >>|- saveFile k (foldr joinWithNewline "" v)) (return ()) c
	>>|-	
		((enterChoiceWithShared "" [ChooseFromGrid id] errorStore) 
	>&>
	(\a.(viewSharedInformation "" [] a ||- (offerSolutions editorStore a)))) <<@ ApplyLayout (arrangeHorizontal) >>|- return () ) <<@ (Title ("Errors & Warnings"))  


offerSolutions ::(Shared (EditorInfo,Map String [String])) (ReadOnlyShared (Maybe String)) -> Task ()
offerSolutions editorstore error = forever ((viewSharedInformation "" [] error ||- watch error) >>*
	[ OnValue (ifValue (\mstr. isJust mstr) (\(Just err). jumpToLine editorstore (getErrorLineNr err) >>| case diagnose err of
		UndefinedVar filename i var = sb "" [(Action ("Create "+++var), createVarSolution filename editorstore i var)] err
		Unknown filename i = sb filename [] err
		))])
	where
	sb message l err = showButtons message l err error (offerSolutions editorstore error) */