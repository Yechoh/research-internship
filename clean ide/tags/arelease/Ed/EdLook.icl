/*
 * EdLook.icl: the edit window update function
 */

implementation module EdLook

import StdInt, StdBool, StdClass
import StdIOCommon
import StdPicture
import EdVisualText, EdVisualCursor, EdVisualLineNr

//import dodebug
trace_n _ f :== f

// editWindowLook: updating the affected areas is done by updating
// each of the rectangles.

editWindowLook	:: EditState SelectState !UpdateState -> (!*Picture -> *Picture)
editWindowLook editState selectState updateState=:{ updArea, newFrame, oldFrame }
  	= trace_n ("Look",updateState) editWindowLook`
where
	editWindowLook` :: !*Picture -> *Picture
	editWindowLook` picture
//		# picture = traceUpdate updArea picture
		# updArea = cleanUpdate updArea	// hack around object i/o bug...
//		# picture = traceUpdate updArea picture
		# picture = vDrawLineNrs fontInfo text newFrame updArea picture
		# picture = vUpdateText fontInfo text newFrame updArea picture
		# picture = case visible of
			True -> vUpdateCursor visible end height fontInfo text newFrame updArea picture
			_    -> vUpdateSelection selection fontInfo text newFrame updArea picture
		= picture
	(fontInfo,ds1)			= getFontInfo (editState,42)
	(text,ds2)				= getText ds1
	(visible,ds3)			= getCursorVisibility ds2
	(height,ds4)			= getCursorHeight ds3
	(selection=:{end},_)	= getSelection ds4

/*
import StdDebug,dodebug

traceUpdate [] s = s
traceUpdate [h:t] s
	#! s = trace_n h s
	= traceUpdate t s
*/

cleanUpdate [] = []
cleanUpdate [h:t]
	| h.corner1.x == h.corner2.x = cleanUpdate t
	| h.corner1.y == h.corner2.y = cleanUpdate t
	= [h: cleanUpdate t]
