hasPartialValue	:: [] (a -> b) (TaskValue a) -> Maybe b
hasPartialValue ataskb (Value a _) = Just (ataskb a)
hasPartialValue _ _ = Nothing
