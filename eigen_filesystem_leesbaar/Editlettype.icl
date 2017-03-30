implementation module editor

import iTasks
import iTasks.API.Extensions.Editors.Ace

content = sharedStore "content" ""

:: Shortcut = No_shortcut 
			| Ctrl_slash 
			| Ctrl_backslash 
			| Ctrl_Shift_backslash 
			| Ctrl_equals 
			| Ctrl_Shift_equals 
			| Ctrl_b //
			| Ctrl_d
			| Ctrl_Shift_d
			| Ctrl_e
			| Ctrl_Shift_e
			| Ctrl_i
			| Ctrl_l
			| Ctrl_m
			| Ctrl_n
			| Ctrl_o
			| Ctrl_r
			| Ctrl_Shift_r
			| Ctrl_s
			| Ctrl_Shift_s
			| Ctrl_Alt_s
			| Ctrl_Shift_Alt_s
			| Ctrl_w
			| Ctrl_Shift_w

:: EditorRecord = 
	{
		content :: [String],
		prev_time :: Time,
		shortcuts :: [Shortcut],
		position :: (Int,Int),
		selection_position :: (Int,Int),
		theme :: String,
		highlighted :: String,
		readOnly :: Bool
	}

aceEditor :: String -> Editor EditorRecord
aceEditor path_to_language_module = undef

undef = undef
