implementation module FilteredListBox

import StdBool, StdEnum, StdList, StdMisc, StdOrdList, StdTuple, StdFunc
import StdControl, StdControlReceiver, StdId, StdPicture, StdPSt, StdReceiver, StdWindow
import StdControlAttribute
import ioutil

from commondef import strictSeq

::	FilteredListBoxState
	=	{ items			:: ![String]					// All items to be displayed
		, selection		:: ![Index]						// The current selection
		, listboxId		:: !FilteredListBoxId			// The ids related to this list box
		, lineHeight	:: !Int
		, initHeight	:: !Int
		, pen			:: ![PenAttribute]
		, ifilter		:: !{#Char} -> Bool				// the item filter
		, aitems		:: ![String]					// all items (unfiltered)
		}

::	FilteredListBoxId
	=	{	fcontrolId	:: !Id							// The Id of the outmost CompoundControl
		,	freceiverId	:: !R2Id FilteredMessageIn FilteredMessageOut	// The Id of the Receiver2Control that handles message passing
		}

openFilteredListBoxId :: !*env -> (!FilteredListBoxId,!*env)	| Ids env
openFilteredListBoxId env
	# (cid, env)	= openId env
	# (rid,env)	= openR2Id env
	= ({fcontrolId=cid,freceiverId=rid},env)

::	FilteredMessageIn
	=	FInGetSelection										// Request to retrieve current selection
	|	FInSetSelection		[Index]							// Request to set the selection to the given index
	|	FInGetItems											// Request to retrieve all current items
	|	FInAppendItems		[FilteredListBoxItem]			// Request to add items behind the last element
	|	FInCloseAllItems									// Request to remove all current items
	|	FInSetPen			[PenAttribute]					// Request to set control pen
	|	FInGetPen											// Request to get control pen
	| FInSetFilter (String->Bool)
	| FInGetFilter

::	FilteredMessageOut
	=	FOutGetSelection		[(String,Index)]			// Reply to retrieve the current selection
	|	FOutSetSelection									// Reply to set the selection
	|	FOutGetItems			[String]					// Reply to get all items
	|	FOutAppendItems										// Reply to append items
	|	FOutCloseAllItems									// Reply to remove all items
	|	FOutSetPen											// Reply to set the control pen
	|	FOutGetPen			[PenAttribute]					// Reply to get the control pen
	| FOutSetFilter
	| FOutGetFilter (String->Bool)

:: FilteredListBoxItem :== String

:: FilteredListBoxControl ls ps
	= FilteredListBoxControl [FilteredListBoxItem] [Int] FilteredListBoxId [ControlAttribute *(*(FilteredListBoxState,ls),ps)]

instance Controls FilteredListBoxControl
where
	getControlType _ = "FilteredListBoxControl"
	controlToHandles (FilteredListBoxControl items selection listboxId attrs) ps
		#! ((lineHeight,initHeight),ps)	= accScreenPicture (liheights penAtts) ps
		#! (domain,ps)					= calcControlDomain penAtts items ps
		= controlToHandles (imp lineHeight initHeight domain) ps
	where
		liheights pen pic
			# pic				= setPenAttributes pen pic
			# (metrics,pic)		= getPenFontMetrics pic
			# lineHeight		= fontLineHeight metrics
			# initHeight		= metrics.fAscent + metrics.fLeading
			= ((lineHeight,initHeight),pic)
		imp lineHeight initHeight domain
			# (customLook,listboxState)	= customlook listboxState
			=
			{	addLS	= listboxState
			,	addDef	= CompoundControl
					(NilLS)
					(
					[	ControlId			listboxId.fcontrolId
					,	ControlItemSpace	0 0							// No itemspace
					,	ControlHScroll		(altScrollFunction Horizontal 10)
					,	ControlVScroll		(altScrollFunction Vertical lineHeight)
					,	ControlViewDomain	domain
					,	ControlLook			True customLook
					:	listboxAtts
					]
					)
				  :+:	Receiver2 listboxId.freceiverId receiver []
			}
		where
			listboxState =
				{ items			= items
				, selection		= selection
				, listboxId		= listboxId
				, lineHeight	= lineHeight
				, initHeight	= initHeight
				, pen			= penAtts
				, ifilter		= const True
				, aitems		= items
				}

		listboxAtts = map toLBCA (filter isListBoxControlAttribute attrs)
		penAtts = flatten (map getControlPenAtt (filter isControlPen attrs))

		isListBoxControlAttribute :: !(ControlAttribute .ps) -> Bool
		isListBoxControlAttribute (ControlFunction _)		= True
		isListBoxControlAttribute ControlHide				= True
		isListBoxControlAttribute (ControlPos _)			= True
		isListBoxControlAttribute (ControlPen _)			= True
		isListBoxControlAttribute (ControlSelectState _)	= True
		isListBoxControlAttribute (ControlViewSize _)		= True
		isListBoxControlAttribute (ControlOuterSize _)		= True
		isListBoxControlAttribute (ControlResize _)			= True
		isListBoxControlAttribute (ControlMouse _ _ _)		= True
		isListBoxControlAttribute (ControlKeyboard _ _ _)	= True
		isListBoxControlAttribute _							= False
		
		toLBCA (ControlFunction f)		= ControlFunction f
		toLBCA ControlHide				= ControlHide
		toLBCA (ControlPos p)			= ControlPos p
		toLBCA (ControlPen p)			= ControlPen p
		toLBCA (ControlSelectState p)	= ControlSelectState p
		toLBCA (ControlViewSize p)		= ControlViewSize p
		toLBCA (ControlOuterSize p)		= ControlOuterSize p
		toLBCA (ControlResize p)		= ControlResize p
		toLBCA (ControlMouse a b c)		= ControlMouse a b c
		toLBCA (ControlKeyboard a b c)	= ControlKeyboard a b c
		toLBCA _ = abort "FilteredListBox:toLBCA: unsupported ControlAttribute"
			
				
		receiver (FInSetFilter filt) ((listboxState=:{pen,aitems},ls),ps)
			# items = filter filt aitems
			# listboxState = {listboxState & ifilter = filt, items = items}
			// refresh...
			# (newDomain,ps)		= calcControlDomain pen items ps
			# (newLook,listboxState)= customlook listboxState
			# ps					= appPIO (seq
				[ setControlViewDomain customId newDomain
				, setControlLooks [(customId,True,(True,newLook))]
				]) ps
			= (FOutSetFilter,((listboxState,ls),ps))
		where
			customId		= listboxState.listboxId.fcontrolId
		receiver (FInGetFilter) ((listboxState=:{ifilter},ls),ps)
			= (FOutGetFilter ifilter,((listboxState,ls),ps))
		//	Return the current selection:
		receiver FInGetSelection ((listboxState=:{items,selection},ls),ps)
			= (FOutGetSelection (map (\index->(items!!(index-1),index)) selection),((listboxState,ls),ps))
		
		//	Set a new selection:
		receiver (FInSetSelection newSelection) ((listboxState=:{lineHeight,initHeight},ls),ps)
			# listboxState	= {FilteredListBoxState | listboxState & selection=newSelection}
			# (newLook,listboxState)= customlook listboxState
			#! ps			= scrolltosel ps
			#! ps			= appPIO (setControlLooks [(customId,True,(True,newLook))]) ps
			= (FOutSetSelection,((listboxState,ls),ps))
		where
			customId		= listboxState.listboxId.fcontrolId
			singlesel = length newSelection == 1
			selitem = hd newSelection
			scrolltosel ps
				| not singlesel = ps
				# top = (selitem-1) * lineHeight
				# bot = selitem * lineHeight
				# (wdef,ps) = accPIO (getParentWindow customId) ps
				| isNothing wdef = ps
				# wdef = fromJust wdef
				# (exists,frame) = getControlViewFrame customId wdef
				| not exists = ps
				| isNothing frame = ps
				# frame = fromJust frame
				# delta = top - frame.corner1.y
				| delta < 0
					= appPIO (moveControlViewFrame customId {vx=0, vy=delta}) ps
				# delta = bot - frame.corner2.y
				| delta > 0
					= appPIO (moveControlViewFrame customId {vx=0, vy=delta}) ps
				= ps
		
		//	Return the current elements:

		receiver FInGetItems ((listboxState=:{items},ls),ps)
			= (FOutGetItems items,((listboxState,ls),ps))
		
		//	Append elements:
		receiver (FInAppendItems newItems) ((listboxState=:{pen,items,aitems,ifilter,lineHeight,initHeight},ls),ps)
			# listboxState			= {listboxState & items=allItems, aitems = aitems++newItems}
			| length newItems`==0
				= (FOutAppendItems,((listboxState,ls),ps))
			# (newDomain,ps)		= calcControlDomain pen allItems ps
			# (newLook,listboxState)= customlook listboxState
			# (delta,ps)			= scrolltoend newDomain ps
			# ps					= appPIO (seq
				[ setControlLooks [(customId,False,(True,newLook))]
				, setControlViewDomain customId newDomain
				, moveControlViewFrame customId {vx=0, vy=delta}
				]) ps
			= (FOutAppendItems,((listboxState,ls),ps))
		where
			customId				= listboxState.listboxId.fcontrolId
			newItems`				= filter ifilter newItems
			allItems				= items++newItems`

			scrolltoend dom=:{corner2={y=bot}} ps
				# (wdef,ps) = accPIO (getParentWindow customId) ps
				| isNothing wdef
					= (zero,ps)
				# wdef = fromJust wdef
				# (exists,frame) = getControlViewFrame customId wdef
				| not exists || isNothing frame
					= (zero,ps)
				# frame = fromJust frame
				# delta = bot - frame.corner2.y
				= (delta,ps)
		
		//	Remove elements:
		//	Remove all:
		receiver (FInCloseAllItems) ((listboxState=:{listboxId,pen,items,selection,lineHeight,initHeight},ls),ps)
			# listboxState	= {listboxState & items=[], aitems = [], selection=[]}
			# (newDomain,ps)= calcControlDomain pen [] ps
			# (newLook,listboxState)= customlook listboxState
			# ps			= appPIO (seq
								[ setControlViewDomain listboxId.fcontrolId newDomain
								, setControlLooks [(listboxId.fcontrolId,True,(True,newLook))]
								]) ps
			= (FOutCloseAllItems,((listboxState,ls),ps))
		
		// Set control pen:
		receiver (FInSetPen newpen) ((listboxState=:{listboxId,items,pen},ls),ps)
			# pen							= removeDupAtt (newpen++pen)
			# (newDomain,ps)				= calcControlDomain pen items ps
			# ((lineHeight,initHeight),ps)	= accScreenPicture (liheights pen) ps
			# listboxState					=
				{ listboxState
				& pen			= pen
				, lineHeight	= lineHeight
				, initHeight	= initHeight
				}
			# (newLook,listboxState)= customlook listboxState
			# ps			= appPIO (seq
								[ setControlViewDomain listboxId.fcontrolId newDomain
								, setControlLooks [(listboxId.fcontrolId,True,(True,newLook))]
								, setControlScrollFunction listboxId.fcontrolId Vertical (altScrollFunction Vertical lineHeight)
								]) ps
			= (FOutSetPen,((listboxState,ls),ps))
		
		// Get control pen:
		receiver (FInGetPen) ((listboxState=:{pen},ls),ps)
			= (FOutGetPen pen,((listboxState,ls),ps))

		calcControlDomain :: ![.PenAttribute] ![.{#Char}] !*(PSt .a) -> *(!.Rectangle,!*PSt .a);
		calcControlDomain pen items ps
			# (newDomain,ps)	= accPIO (accScreenPicture calc) ps
			= (newDomain,ps)
		where
			calc pic
				# pic				= setPenAttributes pen pic
				# (metrics,pic)		= getPenFontMetrics pic
				# (itemWidths,pic)	= getPenFontStringWidths items pic
				# minWidth			= 0
				# maxWidth			= maxList [minWidth:itemWidths]
				# nrItems			= length items
				# height			= nrItems*(fontLineHeight metrics)
				# newDomain			= {corner1=zero,corner2={x=maxWidth,y=height}} // calculate new domain...
				= (newDomain,pic)

 
removeDupAtt [x:xs] = [x:removeDupAtt (filter (diff x) xs)]
where
	diff (PenSize _)	(PenSize _)		= False
	diff (PenPos _)		(PenPos _)		= False
	diff (PenColour _)	(PenColour _)	= False
	diff (PenBack _)	(PenBack _)		= False
	diff (PenFont _)	(PenFont _)		= False
	diff _ _ = True
removeDupAtt _      = []

//	The look of the custom control lists all items and the current selection
customlook ls=:{items,selection,pen,lineHeight,initHeight}
	= (customlook,ls)
where
  customlook _ {newFrame} pict
	# pict				= setPenAttributes pen pict
	# pict				= unfill newFrame pict
	# (_,pict)			= strictSeq [drawline item \\ item <- items] (initHeight,pict)
	# pict				= strictSeq [drawsel sel \\ sel <- selection] pict
	= pict
  where
	(x1,x2)			= (newFrame.corner1.x,newFrame.corner2.x)
	drawsel i		= hilite {corner1={x=x1,y=(i-1)*lineHeight}, corner2={x=x2,y=i*lineHeight-1}}
	drawline line (y,p)
		= (y+lineHeight,drawAt {x=0,y=y} line p)

//--

flbMouse :: ({#Char} -> .(*(PSt .a) -> *PSt .a)) -> .ControlAttribute *((FilteredListBoxState,.b),*PSt .a);
flbMouse efun = ControlMouse mouseFilter Able (mouse efun)

flbKeyboard :: ({#Char} -> .(*(PSt .a) -> *PSt .a)) -> .ControlAttribute *((FilteredListBoxState,.b),*PSt .a);
flbKeyboard efun = ControlKeyboard keyFilter Able (keyboard efun)

keyFilter :: KeyboardState -> Bool
keyFilter (SpecialKey _ (KeyDown True) _) = True
keyFilter _  = False

keyboard efun (SpecialKey key (KeyDown repeat) {shiftDown,controlDown}) ((lbState=:{selection,items,lineHeight},ls),ps)
	| key == enterKey
		| not hasSelection
			= ((lbState,ls),ps)
		// execute selection
		// shift-execute
		// ? what if mul-selection
//		| shiftDown
//			# ps	= (shiftfuns!!(lastSelection-1)) ps
//			= ((lbState,ls),ps)
		# ps	= efun (items!!(lastSelection-1)) ps
		= ((lbState,ls),ps)
	| key == upKey
		// set selection one earlier
		// if shift - extend selection one up
		// if control ...
		// if control-shift ...
		# newSelection	= if hasSelection (max 1 (lastSelection - 1)) 1
		# lbState		= {lbState & selection = [newSelection]}
		# (newLook,lbState)
						= customlook lbState
		# ps			= scrolltoselection True newSelection ps
		# ps			= appPIO (setControlLooks [(customId,True,(True,newLook))]) ps
		= ((lbState,ls),ps)
	| key == downKey
		// set selection one later
		// if shift - extend selection one up
		// if control ...
		// if control-shift ...
		# newSelection	= if hasSelection (min nrItems (lastSelection + 1)) nrItems
		# lbState		= {lbState & selection = [newSelection]}
		# (newLook,lbState)
						= customlook lbState
		# ps			= scrolltoselection True newSelection ps
		# ps			= appPIO (setControlLooks [(customId,True,(True,newLook))]) ps
		= ((lbState,ls),ps)
	| key == beginKey
		// set selection first
		// if shift - extend selection one up
		// if control ...
		// if control-shift ...
		# newSelection	= 1
		# lbState		= {lbState & selection = [newSelection]}
		# (newLook,lbState)
						= customlook lbState
		# ps			= scrolltoselection True newSelection ps
		# ps			= appPIO (setControlLooks [(customId,True,(True,newLook))]) ps
		= ((lbState,ls),ps)
	| key == endKey
		// set selection last
		// if shift - extend selection one up
		// if control ...
		// if control-shift ...
		# newSelection	= nrItems
		# lbState		= {lbState & selection = [newSelection]}
		# (newLook,lbState)
						= customlook lbState
		# ps			= scrolltoselection True newSelection ps
		# ps			= appPIO (setControlLooks [(customId,True,(True,newLook))]) ps
		= ((lbState,ls),ps)
	# (wstate,ps)	= accPIO (getParentWindow customId) ps
	| isNothing wstate
		= ((lbState,ls),ps)
	# wstate	= fromJust wstate
	# (ok,frame) = getControlViewFrame customId wstate
	| not ok
		= ((lbState,ls),ps)
	| isNothing frame
		= ((lbState,ls),ps)
	# frame = fromJust frame
	# linesOnPage = max 1 (dec ((frame.corner2.y - frame.corner1.y) / lineHeight))
	| key == pgUpKey
		// set selection page-up
		// if shift - extend selection one up
		// if control ...
		// if control-shift ...
		# top = (lastSelection-2) * lineHeight
		# newSelection	= if hasSelection
							(if (top <= frame.corner1.y)		//topLine
								(max 1 (lastSelection - linesOnPage))
								(2 + (frame.corner1.y / lineHeight))	//topOfPage
							)
							1
		# lbState		= {lbState & selection = [newSelection]}
		# (newLook,lbState)
						= customlook lbState
		# ps			= scrolltoselection True newSelection ps
		# ps			= appPIO (setControlLooks [(customId,True,(True,newLook))]) ps
		= ((lbState,ls),ps)
	| key == pgDownKey
		// set selection page-down
		// if shift - extend selection one up
		// if control ...
		// if control-shift ...
		# bot = (inc lastSelection) * lineHeight
		# newSelection	= if hasSelection
							(if (bot >= frame.corner2.y)		//bottomLine
								(min nrItems (lastSelection + linesOnPage))
								(frame.corner2.y / lineHeight)	//bottomOfPage
							)
							nrItems
		# lbState		= {lbState & selection = [newSelection]}
		# (newLook,lbState)
						= customlook lbState
		# ps			= scrolltoselection True newSelection ps
		# ps			= appPIO (setControlLooks [(customId,True,(True,newLook))]) ps
		= ((lbState,ls),ps)
	= ((lbState,ls),ps)
where
	nrItems		= length items
	customId	= lbState.listboxId.fcontrolId
	hasSelection
		| isEmpty selection = False
		= True
	lastSelection = hd selection
	scrolltoselection singlesel selitem ps
		| not singlesel = ps
		# top = (selitem-1) * lineHeight
		# bot = selitem * lineHeight
		# (wdef,ps) = accPIO (getParentWindow customId) ps
		| isNothing wdef = ps
		# wdef = fromJust wdef
		# (exists,frame) = getControlViewFrame customId wdef
		| not exists = ps
		| isNothing frame = ps
		# frame = fromJust frame
		# delta = top - frame.corner1.y
		| delta < 0
			= appPIO (moveControlViewFrame customId {vx=0, vy=delta}) ps
		# delta = bot - frame.corner2.y
		| delta > 0
			= appPIO (moveControlViewFrame customId {vx=0, vy=delta}) ps
		= ps
keyboard _ _ _ = abort "FilteredListBox: unsupported keyboard action"

//	The mouse responds only to MouseDowns:
mouseFilter :: MouseState -> Bool
mouseFilter (MouseDown _ _ _)		= True
mouseFilter _						= False

//	The mouse either sets, adds, or removes items to the selection:
mouse efun (MouseDown pos {shiftDown,controlDown} 1) ((listboxState=:{items,selection,lineHeight,initHeight},ls),ps)
	# listboxState	= {FilteredListBoxState | listboxState & selection=okSelection}
	# (newLook,listboxState)
					= customlook listboxState
	# ps			= appPIO (setControlLooks [(customId,True,(True,newLook))]) ps
	= ((listboxState,ls),ps)
where
	nrItems		= length items
	hasSelection= not (isEmpty selection)
	[prevIndex:_]= selection
	listSelection
		| prevIndex < newIndex
			= [prevIndex..newIndex]
			= [newIndex..prevIndex]
	newIndex	= pos.y/lineHeight+1
	newSelection
		| shiftDown
			| hasSelection
				= removeDup [newIndex:listSelection++selection]
			= [newIndex]
		| controlDown
			| isMember newIndex selection
				= removeMembers selection [newIndex]
			= [newIndex:selection]
		= [newIndex]
	okSelection	= filter (isBetween 1 nrItems) newSelection
	customId	= listboxState.listboxId.fcontrolId

mouse efun (MouseDown pos {shiftDown} _) ((listboxState=:{items,lineHeight,initHeight},ls),ps)
// double click
	# listboxState	= {FilteredListBoxState | listboxState & selection=okSelection}
	# (newLook,listboxState)
					= customlook listboxState
	# ps			= appPIO (setControlLooks [(customId,True,(True,newLook))]) ps
	| newIndex > length listboxState.items
		= ((listboxState,ls),ps)
//	| shiftDown
//		# ps	= (shiftfuns!!(newIndex-1)) ps
//		= ((listboxState,ls),ps)
	# ps	= efun (items!!(newIndex-1)) ps
	= ((listboxState,ls),ps)
where
	customId	= listboxState.listboxId.fcontrolId
	newIndex	= pos.y/lineHeight+1
	nrItems		= length items
	newSelection= [newIndex]
	okSelection	= filter (isBetween 1 nrItems) newSelection
mouse _ _ _ = abort "FilteredListBox: unsupported mouse action"
		

//	The functions below take care of the proper communication with the receiver that
//	belongs to the listbox control.

getFilteredListBoxSelection :: !FilteredListBoxId !(PSt .l) -> (!(!Bool,![(String,!Index)]),!PSt .l)
getFilteredListBoxSelection {freceiverId} pState
	# ((_,maybe_out),pState)	= syncSend2 freceiverId FInGetSelection pState
	| isNothing maybe_out
		= ((False,[]),pState)
	# result					= case (fromJust maybe_out) of
									(FOutGetSelection selection)	-> (True,selection)
									_							-> (False,[])
	| otherwise
		= (result,pState)

setFilteredListBoxSelection :: !FilteredListBoxId ![Index] !(PSt .l) -> PSt .l
setFilteredListBoxSelection {freceiverId} selection pState
	= snd (syncSend2 freceiverId (FInSetSelection selection) pState)

getFilteredListBoxItems :: !FilteredListBoxId !(PSt .l) -> (!(!Bool,![String]),!PSt .l)
getFilteredListBoxItems {freceiverId} pState
	# ((_,maybe_out),pState)	= syncSend2 freceiverId FInGetItems pState
	| isNothing maybe_out
		= ((False,[]),pState)
	# result					= case (fromJust maybe_out) of
									(FOutGetItems items)	-> (True,items)
									_					-> (False,[])
	| otherwise
		= (result,pState)

appendFilteredListBoxItems :: !FilteredListBoxId ![FilteredListBoxItem] !(PSt .l) -> PSt .l
appendFilteredListBoxItems {freceiverId} items pState
	= snd (syncSend2 freceiverId (FInAppendItems items) pState)

setFilteredListBoxPen :: !FilteredListBoxId ![PenAttribute] !(PSt .l) -> PSt .l
setFilteredListBoxPen {freceiverId} pen ps
	= snd (syncSend2 freceiverId (FInSetPen pen) ps)

setFilter :: !FilteredListBoxId (String->Bool) !(PSt .l) -> PSt .l
setFilter {freceiverId} flt ps
	= snd (syncSend2 freceiverId (FInSetFilter flt) ps)

getFilter :: !FilteredListBoxId !(PSt .l) -> (!String->Bool,PSt .l)
getFilter {freceiverId} ps
	# ((_,out),ps) = (syncSend2 freceiverId (FInGetFilter) ps)
	| isNothing out = (const True,ps)
	# out = case (fromJust out) of
				(FOutGetFilter filt)	-> filt
				_						-> const True
	= (out,ps)

exec_next_filtered :: !Bool !FilteredListBoxId (String (PSt .l) -> (PSt .l)) !(PSt .l) -> (PSt .l)
exec_next_filtered next lbId efun ps
	#	((ok,sel),ps)	= getFilteredListBoxSelection lbId ps
	| not ok = ps
	#	((ok,lst),ps)	= getFilteredListBoxItems lbId ps
	| not ok =  ps
	| length lst == 0 = ps
	#	idx				= (if (isEmpty sel) (firsti) (nexti (snd(hd sel)) lst))
	#	ps				= setFilteredListBoxSelection lbId [idx] ps
	#	((ok,sel),ps)	= getFilteredListBoxSelection lbId ps
	| not ok = ps
	| isEmpty sel  = ps
	# path =  fst(hd sel)
	= efun path ps
where
	firsti = 1
	nexti idx lst
		# idx = fun idx
		# idx = normalise idx 1 l l
		= idx
	where
		l = length lst
	fun
		| next = inc
		= dec
	normalise num min max incr
		| num < min = normalise (num+incr) min max incr
		| num > max = normalise (num-incr) min max incr
		= num

//	Auxiliary functions:

isBetween :: x x x -> Bool	| Ord x
isBetween low up x
	= low<=x && x<=up
