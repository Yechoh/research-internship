implementation module extraTaskCombinators

import iTasks

(>>|-) infixl 1 :: (Task a) (Task b) -> Task b | iTask a & iTask b
(>>|-) ma mb = ma >>- \_ -> mb

repeatEverySecond :: (Task ()) -> Task ()
repeatEverySecond task =
							get currentTime 
	>>= \now.				waitForTime2 {Time | now & sec=now.Time.sec+1} 
	>>|-					task
	>>|-					repeatEverySecond task
	
repeatEveryTwoMinutes :: (Task ()) -> Task ()
repeatEveryTwoMinutes task =
							get currentTime 
	>>= \now.				waitForTime2 {Time | now & sec=now.Time.sec+10} //waitForTime2 {Time | now & min=now.Time.min+1} 
	>>|-					task
	>>|-					repeatEveryTwoMinutes task	
	
waitForTime2 :: !Time -> Task Time
waitForTime2 time =
	watch currentTime >>* [OnValue (ifValue (\now -> time < now) return)]