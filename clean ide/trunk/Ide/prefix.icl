implementation module prefix

import StdOverloaded, StdString, StdArray, StdClass, StdInt, StdEnum
import EdSelection, EdMonad, EdClient, EdVisualText, EdVisualCursor
import menubar
import EdCommon

import IdeState
import StdList, StdTuple
import StdWindow
import ioutil

add_prefix_selection :: !*(PSt General) -> *PSt General
add_prefix_selection ps
	# (prefix,ps) = getPrefix ps
	| isEmpty prefix = ps
	# prefix = hd prefix
	= replace_lines_and_redraw (add_prefix prefix) ps
where
	add_prefix prefix text_tab_width line
		= prefix +++ line

rem_prefix_selection :: !*(PSt General) -> *PSt General
rem_prefix_selection ps
	# (prefix,ps) = getPrefix ps
	| isEmpty prefix = ps
	# prefix = hd prefix
	= replace_lines_and_redraw (rem_prefix prefix) ps
where
	rem_prefix prefix text_tab_width string
		| size string < size prefix
			= string
		| string % (0,dec (size prefix)) == prefix
			= string % (size prefix, dec (size string))
		= string

change_prefix_dlog :: !*(PSt General) -> *PSt General
change_prefix_dlog ps
	# (pre,ps) = getPrefix ps
	# pre		= if (isEmpty pre) [ini_prefix] pre
	# (dlogId,ps)	= openId ps
	# (popuId,ps)	= openId ps
	# (cancelId,ps)	= openId ps
	# (err,ps) = openModalDialog Void (ddef pre dlogId popuId cancelId) ps
	= ps
where
	ini_prefix = "//\t"

	ddef pre dlogId popuId cancelId = Dialog "Change Prefix"
			(	PopUpControl [(unfixspecials s,noLS1 f s) \\ s <- pre] 1
				[ControlKeyboard filterReturnKeys Able (noLS1(\_ ->g))
				,ControlDeactivate (noLS g)
				,ControlId popuId
				,ControlWidth (PixelWidth 100)
				]
			:+: ButtonControl "OK" [ControlFunction (noLS (closeWindow dlogId))]
			)
			[WindowId dlogId
			,WindowClose (noLS (closeWindow dlogId))
			,WindowCancel cancelId
			]
	where
		f s ps = setPrefix s ps
		g ps=:{io}
			# (wst,io)	= getWindow dlogId io
			# title		= fromJust (snd (getControlText popuId (fromJust wst)))
			# title`	= fixspecials title
			# io		= openPopUpControlItems popuId 0 [(title,f title`)] io
			# io		= selectPopUpControlItem popuId 0 io
			= setPrefix title` {ps & io=io}

//-- copied from finder.icl ...

fixspecials string
	# string = [c \\ c <-: string]
	# string = fixspecials string
	# string = {c \\ c <- string}
	= string
where
	fixspecials [] = []
	fixspecials [h:t]
		# (h,t) = case h of
						'\\' -> fixspecials` t
						c	-> (c,t)
		# t = fixspecials t
		= [h:t]
	fixspecials` [] = ('\\',[])
	fixspecials` [h:t] = case h of
							'\\' -> ('\\',t)
							't' -> ('\t',t)
							c	-> (c,t)

unfixspecials string
	# string = [c \\ c <-: string]
	= unfixspecials string
where
	unfixspecials [] = ""
	unfixspecials [h:t]
		# h = case h of
						'\\' -> "\\\\"
						'\t' -> "\\t"
						c	-> {c}
		# t = unfixspecials t
		= h+++t

//==

// apply replace_lines_f to lines begin_line..end_line in text

//replace_lines :: (TLine -> TLine) !Int !Int !Text -> Text
replace_lines replace_line_f begin_line 0 text = text
replace_lines replace_line_f begin_line n_lines text
	#	(line,text)		= getLine begin_line text
		line			= replace_line_f line
		(_,_,text)		= updateLine begin_line line text
	= replace_lines replace_line_f (inc begin_line) (dec n_lines) text


n_lines_in_selection {start={row=l1},end={row=l2,col=c2}}
	| c2<=0
		= l2-l1
		= inc (l2-l1)

//replace_lines_and_redraw :: .(Int -> {#Char} -> .{#Char}) !*(PSt *General) -> *PSt *General;
replace_lines_and_redraw :: (Int -> {#Char} -> .{#Char}) !*(PSt *General) -> *PSt *General;
replace_lines_and_redraw shift_selection_f ps
	#	(_,ps)			= sendToActiveWindow localmsg ps
		ps				= mb_update_undoinfo ps
	= ps
where
	localmsg =
		getSelection												>>>= \selection ->
		let ordered_selection	= orderSelection selection
			n_lines				= n_lines_in_selection ordered_selection
		in
		IF (isEmptySelection ordered_selection || n_lines==0)
		THEN
			( skip )
		ELSE
			(
			getText													>>>= \text ->
			getFontInfo												>>>= \{tabSize} ->
			let newt = replace_lines (shift_selection_f tabSize) ordered_selection.start.row n_lines text
				(newsel,newtext) = extend_selection_to_begin_and_end_of_line ordered_selection newt
			in
			setText newtext											>>>
			// set saved state false
			vCenterCursor											>>>
			//vResetViewDomain										>>>
			vChangeSelectionTo {newsel & end = newsel.start}						>>>
			mChangeSelectionTo {newsel & end = newsel.start}						>>>
			vTextUpdate ordered_selection.start n_lines	>>>
			vChangeSelectionTo newsel								>>>
			mChangeSelectionTo newsel								>>>
			// set undo to empty undo...
			setUndoInfo {state = None, action = "", uninfo = NoInfo}	>>>
			setNeedSave True
			)

extend_selection_to_begin_and_end_of_line selection text
	| selection.end.col == 0
		= ({selection & start = {selection.start & col = 0}},text)
	#	lastline			= lastLineNr text
	| selection.end.row < lastline
		= ({start = {selection.start & col = 0}, end = {row = inc selection.end.row, col = 0}},text)
	#	(line,text)			= getLine (dec selection.end.row) text
	= ({start = {selection.start & col = 0}, end = {row = selection.end.row, col = size line}},text)

shift_selection_right :: !*(PSt General) -> *PSt General
shift_selection_right ps
	= replace_lines_and_redraw shift_line_right ps
where
	shift_line_right text_tab_width line
		= "\t" +++ line

shift_selection_left :: !*(PSt General) -> *PSt General
shift_selection_left ps
	= replace_lines_and_redraw shift_line_left ps
where
	shift_line_left :: Int String -> String
	shift_line_left text_tab_width string
		| size string == 0
			= string
		| string.[0] == '\t'
			= string % (1,dec (size string))
		# n_leading_spaces = count_spaces_at_begin 0 string
		# n_leading_spaces = min n_leading_spaces text_tab_width
		| n_leading_spaces == 0
			= string
		= string % (n_leading_spaces,dec (size string))

	count_spaces_at_begin n_spaces string
		| n_spaces < size string && string.[n_spaces] == ' '
			= count_spaces_at_begin (inc n_spaces) string
		= n_spaces
