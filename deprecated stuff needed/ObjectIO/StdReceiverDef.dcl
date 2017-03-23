definition module StdReceiverDef


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdReceiverDef contains the types to define the standard set of receivers.
//	********************************************************************************


import	StdIOCommon


::	Receiver  m   ls pst = Receiver  (RId  m)   (ReceiverFunction  m   *(ls,pst))
                                                [ReceiverAttribute     *(ls,pst)]
::	Receiver2 m r ls pst = Receiver2 (R2Id m r) (Receiver2Function m r *(ls,pst))
                                                [ReceiverAttribute     *(ls,pst)]

::	ReceiverFunction  m   st :== m -> st ->     st
::	Receiver2Function m r st :== m -> st -> *(r,st)

::	ReceiverAttribute	st					// Default:
	=	ReceiverInit		(IdFun st)		// no actions after opening receiver
	|	ReceiverSelectState	SelectState		// receiver Able
	|	ReceiverConnectedReceivers [Id]		// []
::	ReceiverType
	:==	String
