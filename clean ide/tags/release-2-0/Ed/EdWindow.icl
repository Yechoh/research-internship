implementation module EdWindow

// opening and closing editor windows

import StdInt, StdClass, StdList
import StdWindow, StdControlReceiver, StdIOBasic
import EdMessage, EdMonad, EdLook, EdVisualText, EdState
import ioutil

//---

openEditWindow :: Id .Title String Text !Font (Int,Bool,Bool,Bool,Bool) SyntaxColours Id [.WindowAttribute *(EditState,*PSt *b)] !*(PSt *b) -> !*PSt *b | Editor b
openEditWindow uId title pathName text font tabs syncols windowId atts ps
  # (editorState,ps)				= getEditorState ps
  // generate a receiver identifier and store it in the editor state
  # (editId, ps)					= openEditId ps
	editorState						= addReceiver windowId editId editorState
  // create the local state of the editor window
  # (editState, ps)					= initEditState windowId uId pathName font tabs syncols ps
	(_, (editState, ps))			= setText text (editState, ps)
	(fontInfo, (editState, ps))		= getFontInfo (editState, ps) 
  // compute the view domain of the visual text
	(viewDomain, (editState, ps))	= computeViewDomain (editState, ps)
  // setup the window attributes 
	editLook						= editWindowLook editState
	windowAttrs						= atts ++		// in this order so that new attributes override default atts
										  	[ WindowViewSize   { w = 800, h = fontInfo.FontInfo.lineHeight * 40 }
											, WindowHMargin	   0 0
											, WindowVMargin	   0 0
											, WindowId		   windowId
											, WindowViewDomain viewDomain
											, WindowLook	   True editLook
											, WindowHScroll	   (altScrollFunction Horizontal fontInfo.metrics.fMaxWidth)
											, WindowVScroll	   (alignScrollFunction Vertical fontInfo.FontInfo.lineHeight)
											, WindowPos		   (Fix, OffsetVector {vx=10, vy=10})
											, WindowCursor	   IBeamCursor
											]
	// create the receiver that will be part of the
	// editor window. The receiver can be used to change features
	// of the editor from the outside (features like font, text)
  # receiver	 					= openEditReceiver editId
	// create and open the window
  # window		 					= Window title receiver windowAttrs
  # (_,ps)	 						= openWindow editState window ps
  # ps		 						= setEditorState editorState ps
  = ps
  
closeEditWindow :: !Id !*(PSt *b) -> *PSt *b | Editor b;
closeEditWindow windowId pState
  # (editorState,pState) 		= getEditorState pState
  # (maybeEditId, editorState)	= findReceiver windowId editorState
  | isNothing maybeEditId
  	= setEditorState editorState pState
  #	editorState					= removeReceiver windowId editorState
	pState						= closeWindow windowId pState
  = setEditorState editorState pState
