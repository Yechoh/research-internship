definition module descriptionAndOptions

import iTasks

doubleEnterChoiceWithShared :: d [ChoiceOption a] (ReadWriteShared [a] w) (a->d) (a->[ChoiceOption b]) (a->[b])-> Task a | toPrompt d & iTask a & iTask w & iTask b

//viewSharedListWithDescriptionAndOptions :: (Shared [a]) (a -> c) (a -> String) (a -> [(String,Task b)]) -> Task b | iTask a & iTask b & iTask c & == c

//viewListWithDescriptionAndOptions :: [c] [String] [[(String,Task b)]] -> Task b | iTask b & iTask c

