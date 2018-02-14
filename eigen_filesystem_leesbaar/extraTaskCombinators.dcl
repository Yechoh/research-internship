definition module extraTaskCombinators

import iTasks

//this combinator is a merge of >>- and >>|
(>>|-) infixl 1 :: (Task a) (Task b) -> Task b | iTask a & iTask b

//calls the given task every second
repeatEverySecond :: (Task ()) -> Task ()
repeatEveryTenSeconds :: (Task ()) -> Task ()

//very similar to waitForTime in taskCombinators
//given a time, it waits until that time
//contrary to waitForTime, it does not create a window
waitForTime2 :: !Time -> Task Time
