module Main

import iTasks
import qualified Data.Map as DM
import iTasks.UI.Editor.Builtin

// Data ////////////////////////////////////////////////////////////////////////

:: User_type = Customer | Worker

:: Request = 
	{
		contactInformation :: String,
		complaintRequest :: String
	}	

:: Status = Open | Approved String | Denied String
:: Process = {processor :: String, details :: String, status :: Status}

:: Complaint = 
	{
		index :: Int,
		datetime :: DateTime,
		name :: String,
		contactInformation :: String,
		complaintRequest :: String,
		additionalDetails :: String,
		status :: Status,
		process :: [Process]
	}


derive class iTask User_type, Request, Complaint, Process, Status

instance == Status where
	(==) a b = False

// Stores //////////////////////////////////////////////////////////////////////

employeenamesStore :: Shared [String]
employeenamesStore = sharedStore "employeenames" []

complaintsStore :: Shared [Complaint]
complaintsStore = sharedStore "complaints" []

complaintsIndex :: Shared Int
complaintsIndex = sharedStore "complaintsIndex" 0

archiveStore :: Shared [Complaint]
archiveStore = sharedStore "archive" []

// Tasks ///////////////////////////////////////////////////////////////////////

(>>|-) infixl 1 :: (Task a) (Task b) -> Task b | iTask a & iTask b
(>>|-) ma mb = ma >>- \_ -> mb

enterComplaint :: String -> Task ()
enterComplaint name = enterInformation "Do you want to fill in a complaint request?" [] >>= 
	\req. get currentDateTime >>-
	\datetime. upd (\i.i+1) complaintsIndex >>=
	\index.	upd (\cs. [{Complaint|index,datetime,name,contactInformation=req.Request.contactInformation,complaintRequest=req.Request.complaintRequest,additionalDetails="",status=Open,process=[]}:cs]) complaintsStore >>|-
	enterInformation "Can you give additional details?" []
	>>* [	OnAction (ActionCancel) (always(enterComplaint name))
		,	OnAction (Action "Save") (hasValue(
				\ad. upd (update (\c2. c2.Complaint.index==index) (\c2.{c2 & additionalDetails = ad})) complaintsStore
				>>|- enterComplaint name))
		]

evaluateProcess :: Int String -> Task ()
evaluateProcess i name = viewSharedInformation "Complaint" [ViewAs ctje] complaintsStore /*||-
	enterChoiceWithShared "" [ChooseFromList \i.i] (filter (\p. p.Process.status=Open) (filter \c. c.Complaint.status==Open)) complaintsStore*/
	>>* [	OnAction (Action "Approve process") (always(
				get complaintsStore >>-
				\cs.upd (\l.[{Complaint| (ctje cs) & process = [{Process| (hd (ctje cs).Complaint.process) & status=Approved name}: tl (ctje cs).Complaint.process]}:l]) archiveStore >>|-
				upd (filter (\c. c.Complaint.index <> i)) complaintsStore >>|-
				handleApprovedComplaints name))
		,	OnAction (Action "Deny process") (always( upd (update (\c. c.Complaint.index == i) (\c. {Complaint| c & process = [{Process| (hd c.Complaint.process) & status=Denied name}: tl c.Complaint.process]})) complaintsStore >>|- handleApprovedComplaints name))
		,	OnAction (Action "Cancel") (always (handleApprovedComplaints name))
		]
		where ctje cs = hd (filter (\c. c.Complaint.index == i) cs)

processComplaint :: Int String -> Task ()
processComplaint i name = viewSharedInformation "Complaint" [ViewAs (\l.hd (filter (\c. c.Complaint.index == i) l))] complaintsStore 
	||- enterInformation "Describe your processing steps" [EnterUsing id (textArea 'DM'.newMap)]
	>>* [	OnAction (Action "Continue") (hasValue(\details. upd (update (\c. c.Complaint.index == i) (\c. {c & process = [{processor=name,details=details,status=Open}:c.Complaint.process]})) complaintsStore>>|- handleApprovedComplaints name))
		,	OnAction (Action "Cancel") (always (handleApprovedComplaints name))
		]

isOpen :: Status -> Bool
isOpen s = case s of 
	Open = True
	otherwise = False

isApproved :: Status -> Bool
isApproved s = case s of
	 Approved _  = True
	 otherwise = False
	
isDenied :: Status -> Bool
isDenied s = case s of
	 Denied _ = True
	 otherwise = False


handleApprovedComplaints :: String -> Task ()
handleApprovedComplaints name = enterChoiceWithShared "" [ChooseFromList \i.i] (mapRead (filter \c. isApproved c.Complaint.status) complaintsStore) 
	>>* [OnAction (Action "refresh") (always (handleApprovedComplaints name))
		,OnAction (ActionContinue) (hasValue(
		\c. if (isEmpty c.Complaint.process) 
			(processComplaint c.Complaint.index name)
			if (isOpen ((hd (c.Complaint.process)).Process.status))
				if ((hd c.Complaint.process).Process.processor == name)
					(viewInformation "You cannot evaluate your own process" [] c
					>>| handleApprovedComplaints name)
					(evaluateProcess c.Complaint.index name)
				(processComplaint c.Complaint.index name)
				
		))]

evaluateOpenComplaint :: Int String -> Task ()
evaluateOpenComplaint i name = viewSharedInformation "Complaint" [] (mapRead (\l.hd (filter (\c. c.Complaint.index == i) l)) complaintsStore) 
	>>* [	OnAction (Action "Approve complaint") (always( upd (update (\c. c.Complaint.index == i) (\c. {Complaint| c & status = Approved name})) complaintsStore>>|- evaluateOpenComplaints name))
		,	OnAction (Action "Deny complaint") (always( upd (update (\c. c.Complaint.index == i) (\c. {Complaint| c & status = Denied name})) complaintsStore >>|- evaluateOpenComplaints name))
		,	OnAction (Action "Cancel") (always (evaluateOpenComplaints name))
		]

/*
complaint2PrettyComplaint :: Complaint -> PrettyComplaint
complaint2PrettyComplaint {index,name,contactInformation,complaintRequest,additionalDetails,status} =
	{PrettyComplaint|
		name,contactInformation,complaintRequest,additionalDetails,
		status= case status of
			| Open = "Open"
			| Approved approver [] = "Approved by " +++ approver
			| Approved approver l = prettyProcesslist True l
	}
	where
		prettyProcesslist b [] = ""
		prettyProcesslist True [x:y] = "Processed by " +++ x +++ prettyProcesslist False y
		prettyProcesslist False [x:y] = ", but denied by " +++ x +++ ". " +++ prettyProcesslist True y 
*/
	
evaluateOpenComplaints :: String -> Task ()
evaluateOpenComplaints name = enterChoiceWithShared "" [ChooseFromList \i.i] (mapRead (filter (\c. isOpen c.Complaint.status || isDenied c.Complaint.status)) complaintsStore)
	>>* [OnAction (Action "refresh") (always (evaluateOpenComplaints name))
		,OnAction ActionContinue (hasValue (\c. evaluateOpenComplaint c.index name))
		]

viewArchive :: Task ()
viewArchive = 
	get currentDateTime >>-
	\now. get complaintsStore >>-
	\cs. upd (\l.( l ++ (filter (archivable now) cs))) archiveStore >>|-
	upd (filter \c.(not (archivable now c))) complaintsStore >>|-
	viewSharedInformation "" [] archiveStore >>|- return ()
		where
		archivable now c = isDenied c.Complaint.status && (c.Complaint.additionalDetails <> "" || c.Complaint.datetime>{DateTime| now & mon=now.DateTime.mon+1})
	
requestOverview :: String -> Task ()
requestOverview name = parallel 
	[(Embedded,(\tasklist.(evaluateOpenComplaints name)<<@ Title "open")),
	 (Embedded,(\tasklist.(handleApprovedComplaints name)<<@ Title "approved")),
	 (Embedded,(\tasklist.(viewArchive)<<@ Title "archive")),
	 (Embedded,(\tasklist.(viewSharedInformation "" [] employeenamesStore>>|-return ())<<@ Title  "Good Workers"))
	 ]
	[] <<@ ApplyLayout (layoutSubs SelectRoot arrangeWithTabs)
	>>* [OnAction (Action "log out") (always log_in)]

update :: (a->Bool) (a->a) [a] -> [a]
update pred f [] = []
update pred f [a:r]
	| pred a = [f a: update pred f r]
	| otherwise = [a: update pred f r]

fillQuestionaire :: Complaint -> Task ()
fillQuestionaire c = enterInformation ("Can you give additional details for your complaint request: " +++ c.Complaint.complaintRequest +++ "?") [EnterUsing id (textArea 'DM'.newMap)]
	>>* [	OnAction (ActionCancel) (always(return ()))
		,	OnAction (Action "Save") (hasValue(
				\ad. upd (update (\c2. c2.Complaint.index==c.Complaint.index) (\c2.{c2 & additionalDetails = ad})) complaintsStore
				>>|- return ()))
		]

setadd :: a [a] -> [a] | == a
setadd a [] = [a]
setadd a [x:y]
	| a==x = y
	| a<>x = [x: setadd a y]

workerPage :: Task ()
workerPage = enterInformation "Enter your name" [] >>=
	\name. upd (setadd name) employeenamesStore >>|-
	requestOverview name

customerPage :: Task ()
customerPage = enterInformation "Enter your name" [] >>= 
	\name. enterComplaint name -|| openQuestionaires name
	>>* [OnAction (Action "log out") (always (log_in))]
	where
		openQuestionaires :: String -> Task [(TaskTime,TaskValue ())]
		openQuestionaires name = get complaintsStore >>- \cs. parallel (map (\complaint. (Embedded,\tasklist.fillQuestionaire complaint)) (filter (\complaint. complaint.Complaint.name==name) cs)) []
			>>* [OnAction (Action "refresh") (always (openQuestionaires name))]
			
log_in :: Task ()
log_in = enterInformation "Are you a?" [] >>= \ut. case ut of
	  Customer = customerPage
	  Worker = workerPage

// Main ////////////////////////////////////////////////////////////////////////

main :: Task ()
main =
    return ()

Start :: *World -> *World
Start world = startEngine log_in world
