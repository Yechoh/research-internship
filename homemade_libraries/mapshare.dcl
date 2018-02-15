definition module mapshare

import iTasks
import qualified Data.Map as DM

//get element of a shared map. shows 'not found' when key does not match.
msget :: a (Shared (Map a b)) -> Task b | iTask b & zero b & < a & iTask a

//get maybe element of a shared map.
msgetm :: a (Shared (Map a b)) -> Task (Maybe b) | iTask b & < a & iTask a

//get shared element of a shared map. gives the zero-form of the element if the element is removed from the shared map
msgets :: a (Shared (Map a b)) -> (Shared b) | zero b & < a

//get shared maybe element of a shared map.
msgetsm :: a (Shared (Map a b)) -> (Shared (Maybe b)) | < a

//adds or replaces an element in a shared map
msput :: a b (Shared (Map a b)) -> Task (Map a b) | iTask a & iTask b & < a

//adds or replaces a maybe element in a shared map, if the maybe is a Just.
msputm :: a (Maybe b) (Shared (Map a b)) -> Task (Map a b) |  iTask a &  iTask b & < a

//replaces an existing element in a shared map. Does nothing if it does not already exist.
msset :: a b (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a

//replaces an existing element in a shared map. Does nothing if it does not already exist or if the maybe is Nothing
mssetm :: a (Maybe b) (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a

//adds an element to a shared map. Does not replace the element if it already exists.
msadd :: a b (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a

//adds an element to a shared map. Does not replace the element if it already exists or if the maybe is Nothing.
msaddm :: a (Maybe b) (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a

//deletes an element from a shared map
msdel :: a (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a

//updates an existing element in a shared map. Does nothing if it does not already exist.
msupd :: a (b -> b) (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a

//gives the size of a shared map
msmapSize :: (Shared (Map a b)) -> Task Int |  iTask a &  iTask b & < a

//gives a readOnlyshared of the size of the shared map
msmapSizes :: (Shared (Map a b)) -> (ReadOnlyShared Int) | < a
