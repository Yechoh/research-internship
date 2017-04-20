implementation module test

import iTasks

myTest :: (a -> Bool) (Task a) -> (Task (Maybe a)) | iTask a

myTest pred taska
    =       taska
    >>*     [ OnAction (Action  "Ok")     (ifValue pred (return o Just))
            ,  OnAction (Action "Not Ok") (ifValue (not o pred) (\_ -> return Nothing))
            ]




