module test_watch

import iTasks
import qualified Data.Map as DM
import iTasks.UI.Definition, iTasks.UI.Editor.Builtin

waitForTime2 :: !Time -> Task Time
waitForTime2 time =
	watch currentTime >>* [OnValue (ifValue (\now -> time < now) return)]
	
thetask :: Task ()
thetask = get currentTime >>=
			\now.  waitForTime2 {Time| now & sec=now.Time.sec+1} >>- \_ -> set ((toString now)+++ "\n\n\n\n\n\n haha") state >>| thetask
			
Start :: *World -> *World
Start world = startEngine (viewSharedInformation "hello" [ViewUsing id (textArea 'DM'.newMap)] state -&&- thetask) world

state = sharedStore "state" ""