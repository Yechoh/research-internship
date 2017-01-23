/*
 * EdMessage.dcl: message passing with the editor
 */

definition module EdMessage

from StdId			import Id,RId,Ids
from StdPSt			import PSt, IOSt
from StdReceiver	import Receiver2, R2Id, Receiver2Function, ReceiverAttribute
from EdMonad		import EditState

:: EditId
:: Message

openEditId			:: *env								-> (EditId, *env) | Ids env
openEditReceiver	:: !EditId							-> Receiver2 Message Message EditState (PSt *l)

hasEditState		:: !EditId			  !*(PSt *l)	-> *(Bool,		*PSt *l)
getEditState		:: !EditId			  !*(PSt *l)	-> *(EditState,	*PSt *l)
setEditState		:: !EditId !EditState !*(PSt *l)	->				*PSt *l

