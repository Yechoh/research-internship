definition module doubleEnterChoice

import iTasks

//doubleEnterChoiceWithShared :: d [ChoiceOption a] (ReadWriteShared [a] w) (a->d) (a->[ChoiceOption b]) (a->[b])-> Task b | toPrompt d & iTask a & iTask w & iTask b


chooseTaskBasedOnElementOfSharedList :: d (Shared [a]) (a->d2) (a->[a2]) (a a2 ->Task ()) -> Task () | toPrompt d & iTask a & toPrompt d2 & iTask a2 & ==a

chooseTaskBasedOnFeed :: (Task a) (a->d2) (a->[a2]) (a a2 ->Task ()) -> Task () | iTask a & toPrompt d2 & iTask a2

chooseTaskBasedOnFeed2 :: (Task a) (a->d2) (a->Task [a2]) (a2->d3) (a a2 ->Task ()) -> Task () | iTask d3 & iTask a & toPrompt d2 & iTask a2
