definition module doubleEnterChoice

import iTasks

doubleEnterChoiceWithShared :: d [ChoiceOption a] (ReadWriteShared [a] w) (a->d) (a->[ChoiceOption b]) (a->[b])-> Task a | toPrompt d & iTask a & iTask w & iTask b
