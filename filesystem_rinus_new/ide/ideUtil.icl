implementation module ideUtil

import iTasks
import Text.HTML

(>>|-) infixl 1 :: (Task a) (Task b) -> Task b | iTask a & iTask b
(>>|-) ma mb = ma >>- \_ -> mb

mbCancel :: (Task a) -> Task ()
mbCancel ta 
	=
	(		(ta @! ())
	-||- 	(	viewInformation () [] ()
			>>*	[OnAction ActionCancel (always (return ()))]
			)
	)
	>>|- return () // Bas: why do I need this additional return over here

jumpTo :: !String !String -> Task ()
jumpTo text page  
	= viewInformation () []  (ATag [HrefAttr page, TargetAttr "_blank"] [Text text])
	>>| return ()

// Try outs

optionallyHidden :: !String !Bool !(Task a) -> Task a | iTask a
optionallyHidden taskName initiallyShown task
	=  	updateChoice () [] [choiceShow,choiceHide] (if initiallyShown choiceHide choiceShow) 	// The task of indicating your visibility preference
    ||- task 																					// The actual work you are doing
    //    <<@ ApplyLayout hideByValue //The actual hiding/showing is done by a dynamic layout rule

where
  choiceShow = "Show " +++ taskName
  choiceHide = "Hide " +++ taskName

showOrHide2 ::  !String !Bool !(Task a) -> Task a | iTask a
showOrHide2 taskName show task 
	| show		=  	updateChoice () [] [choiceShow,choiceHide] choiceShow 	// The task of indicating your visibility preference
   					-|| task 																					// The actual work you are doing
				>>* [OnValue (ifValue ((==) choiceHide) (\_ -> showOrHide2 taskName False task))]
	| not show  =	
				(	updateChoice () [] [choiceShow,choiceHide] choiceHide 	// The task of indicating your visibility preference
 				>>* [OnValue (ifValue ((==) choiceShow) (\_ -> showOrHide2 taskName True task))]
 				) <<@ ApplyLayout frameCompact

where
  choiceShow = "Show " +++ taskName
  choiceHide = "Hide " +++ taskName

showOrHide :: !String !Bool !(Task ()) -> Task ()
showOrHide actionName show task
//	=			task // toggle show							// does not really work as wanted...
	=	toggle show							
	where
		toggle show 
			=		if show task (viewInformation () [] () <<@ NoUserInterface)
			>>*		[OnAction (Action ((if show "Hide " "Show ") +++ actionName)) (always (toggle (not show)))]


// Try out 

import ideStores, Internet.HTTP

parmIdStore :: Shared (Maybe TaskId)							// temp hack to pass info from one page to another
parmIdStore  = sharedStore "parameterIdPassing" Nothing

evalInBrowserPage :: String (Task a) Bool  -> Task () | iTask a
evalInBrowserPage prompt task newPage  
	= 		evalInBrowserPage`  <<@ InWindow @! ()
where
	evalInBrowserPage` 
		=				get currentUser
		>>= \me ->		appendTopLevelTaskFor me True task
		>>= \taskId ->	set (Just taskId) parmIdStore									// store parameter for to be read by page called
		>>|	mbCancel
				(		viewInformation prompt [] showLink								// temp fixed, no such action buttons yet  
				||-		wait () (\parm -> isNothing parm) parmStore @! ()				// wait until parameter has been read by called page
				)
		>>| return ()

	showLink = (ATag [HrefAttr "/evalTask", TargetAttr (if newPage "_blank" "_inline")] [Text prompt])



evalMyTask :: HTTPRequest -> Task ()
evalMyTask info														
	= 				viewInformation "Currently no parameters passed in this way" [] info.arg_get
	>>|				get parmIdStore
	>>- \parm ->   	case parm of
						Nothing 	->	return ()						// only works once for the time-being, 			
						Just taskId	->	workOn taskId @! ()

