implementation module EdMouse

//*********************************************************************************
// Original Clean Library Software Module
// Written for Clean version  : 1.3
// Written for I/O version    : 1.2
// Author                     : Diederik van Arkel
// Date                       :
// Last Modified by           :
// Date                       :
// Copyright                  : 1999 Hilt - High Level Software Tools B.V.
//                            : University of Nijmegen
// e-mail                     : clean@cs.kun.nl or rinus@hilt.nl
//*********************************************************************************
// It is allowed to modify this module for your own purposes but it is NOT allowed
// to (re)distribute the code or the modified code in ANY form without written
// permission.
//*********************************************************************************

/*
 * EdMouse.icl: handling mouse events
 */

import StdClass, StdInt, StdBool, StdArray, StdChar, StdList, StdTuple
import StdIOCommon, StdPSt, StdWindow
import EdVisualCursor, EdVisualText, EdSelection

import EdAction, EdMonad, EdCommon
import Platform

platformCommand mods
	:== PlatformDependant mods.controlDown mods.commandDown

/* editWindowMouse wraps the monadic mouse function, so that the type
 * conforms to that of a call-back function in the Object I/O library
 */
/*
editWindowMouse :: Tree MouseState (!EditState, !PSt PLocState) -> (EditState, PSt PLocState)
editWindowMouse tree mouseState state
  # state = p4mouse tree mouseState state					// P4
*/
editWindowMouse :: MouseState (!EditState, !PSt PLocState) -> (EditState, PSt PLocState)
editWindowMouse mouseState state
  = noResult
	  (
	  	setVirtualX 0	>>>
	  	mouse mouseState
	  ) state 

mouse :: MouseState -> EditMonad (PSt PLocState) nothing
mouse (MouseDown point mods=:{shiftDown} nrDown)
  = vHideCursor					>>>
    getText						>>>= \text ->
    getFontInfo					>>>= \fontInfo ->
	let
		position = pointToPosition point text fontInfo
	in
	IF ( platformCommand mods )
	THEN
	  ( case nrDown of
			1 -> singleClick position
			2 -> controlDoubleClick shiftDown position
			3 -> tripleClick position
			_ -> skip
	  )
	ELSE
	(
	IF shiftDown
	THEN 
	  ( shiftClick position )
	ELSE
	  ( case nrDown of
			1 -> singleClick position
			2 -> doubleClick position
			3 -> tripleClick position
			_ -> skip
	  )
	)        	  
mouse (MouseDrag point modifiers)		 // mouse drag
  = getSelection						>>>= \selection -> 
	getText								>>>= \text ->
	getFontInfo							>>>= \fontInfo ->
	let
		newPosition = pointToPosition point text fontInfo
	in
	getSelectMode						>>>= \selectMode ->
	
	// change the selection depending on the current selection mode
	(case selectMode of 
		SelectChars ->
			vChangeSelectionTo {start=selection.start, end=newPosition} 

		SelectWords firstWord ->
			selectWordAt newPosition		>>>= \endSelection ->
			
			IF (newPosition < firstWord.end)
			THEN
			  ( vChangeSelectionTo { start=firstWord.end
			  					   , end=endSelection.start
			  					   }
			  )
			ELSE
			  (
			  	vChangeSelectionTo { start=firstWord.start
			  					   , end=endSelection.end
			  					   }
			  )
		
		SelectLines firstLine ->
			getText						>>>= \text ->
			vChangeSelectionTo (selectLines firstLine newPosition.row text)
	)									>>>
	vScrollToCursor

mouse (MouseUp point modifiers)					   // mouse up handling
  = getSelection						>>>= \selection ->
    mChangeSelectionTo selection		>>>
    IF (isEmptySelection selection)
    THEN ( vShowCursor )
    ELSE ( skip )

mouse _											   // ignore other mouse events
  = skip

// single click
//singleClick :: .Position -> .(*(EditState,*PSt *General) -> *(b,*(EditState,*PSt *General)));
singleClick position =
	vChangeSelectionTo 
		{start=position,end=position}			>>>
	setSelectMode SelectChars					>>>
    getUndoInfo                                     >>>= \undoinfo ->
    case undoinfo.uninfo of
    (InsertInfo True state) -> setUndoInfo {undoinfo & uninfo=(InsertInfo False state)}
    (RemoveInfo True state) -> setUndoInfo {undoinfo & uninfo=(RemoveInfo False state)}
    _ -> skip

// double click
//doubleClick :: .Position -> .(*(EditState,*PSt *General) -> *(b,*(EditState,*PSt *General)));
doubleClick position =
	selectWordAt position						>>>= \selection ->
	vChangeSelectionTo selection				>>>
	setSelectMode (SelectWords selection)

// triple click
//tripleClick :: .Position -> .(*(EditState,*PSt *General) -> *(b,*(EditState,*PSt *General)));
tripleClick position=:{row} =
	getText										>>>= \text ->
	vChangeSelectionTo
		 (selectLines row row text)				>>>
 	setSelectMode (SelectLines row)

// shift click
//shiftClick :: .Position -> .(*(EditState,*PSt *General) -> *(b,*(EditState,*PSt *General)));
shiftClick position =
	getSelection								>>>= \selection ->
	vChangeSelectionTo
		{start=selection.start, end=position}

//--

noMouseMoved :: !MouseState -> Bool
noMouseMoved (MouseMove _ _) = doP4
noMouseMoved _				 = True

//-- P4 --- !!! ZITTEN OOK NOG WIJZIGINGEN IN EdMonad !!!

doP4 :== False
noP4 :== True
/*
import StdTimer,tooltip,PmParse,UtilStrictLists,EdFile,treeparse

p4mouse boom mouseState state
  | noP4 = state
  # (winId,state)	= getWindowId state								// P4
  # (timId,state)	= getTimerId state								// P4
  # pos				= getMouseStatePos mouseState					// P4
  # (oldpos,state)	= getToolPt state								// P4
  # state			= case (pos == oldpos) of						// P4
  						True	-> state							// P4
  						_		-> p4fun boom timId winId pos state		// P4
  # (_,state)		= setToolPt pos state							// P4
  = state															// P4

p4fun boom timerId windowId pos state
    # (text,state)		= getText state
    # (fontInfo,state)	= getFontInfo state
	# position			= pointToPosition pos text fontInfo
	# (sel,state)		= selectWordAt position state
	# orderedSelection	= orderSelection sel
	# (fragment, _)		= getTextFragment orderedSelection text
	# str				= stringsToString fragment
	# (es,ps)			= state
	# ps				= appPIO (closeTimer timerId) ps
	# (_,ps)			= openTimer Void (tdef boom timerId windowId pos str) ps 
	= (es,ps)

tdef boom timerId winId pos str = Timer (1 * ticksPerSecond) NilLS [TimerId timerId,TimerFunction tfunctie]
where
	tfunctie nr_of_intervals (ls,ps)
//		# (def,ps)	= findDefinition str ps
		# def		= searchTree str boom
		# ps		= ShowToolTip winId pos.x pos.y def ps
		# ps		= appPIO (closeTimer timerId) ps
		= (ls,ps)

//    # (boom, ps) 	= readFileInTree ps

findDefinition str ps
	# mod				= "C:\\Projects\\CleanIDE\\Ed\\EdText.dcl"
	# ((imp,pos),ps)	= accFiles (FindDefinitionInFile False Nil str mod) ps
	# found = case pos of
		(Pos ln cn tl)	-> Just ln
		(Cls ln cn tl)	-> Just ln
		_				-> Nothing
	| isNothing found
		= (str+++" not found ",ps)
	# ln				= fromJust found
	# ((txt,_,_),ps)	= readText mod ps
	| isError txt
		= (mod+++" not read ",ps)
	# txt				= fromOk txt
	# (line,txt)		= getLine ln txt
	= (line,ps)
*/
