implementation module mapshare

import iTasks
import qualified Data.Map as DM

//derive class iTask Map

msget :: a (Shared (Map a b)) -> Task b | iTask b & zero b & < a & iTask a
msget key smap = get smap >>- \map.
    case ('DM'.get key map) of
        Nothing = viewInformation "not found" [] (key) >>| return zero
        Just el = return el

msgetm :: a (Shared (Map a b)) -> Task (Maybe b) | iTask b & < a & iTask a
msgetm key smap = get smap >>- \map. return ('DM'.get key map)

msgets :: a (Shared (Map a b)) -> (Shared b) | zero b & < a
msgets key smap = mapReadWrite (get,put) smap
	where
		//get :: (Map a b) -> (b)
		get map = maybe zero id ('DM'.get key map)

		//put :: (b) (Map a b) -> Maybe (Map a b) | zero b & < a
		put el map = Just ('DM'.put key el map)

maptoel:: a (Map a b) (b) -> (b) | < a & iTask b & zero b & iTask a
maptoel key map el = maybe el id ('DM'.get key map)

eltomap :: a (b) (Map a b) -> (Map a b) | < a & iTask b & zero b & iTask a
eltomap key el map = case 'DM'.get key map of
    Nothing = map
    Just el2 = 'DM'.put key el map

temp :: (Shared b) a (Shared (Map a b)) -> (Shared (Map a b),Shared b) | < a & iTask b & zero b & iTask a
temp sel key smap = symmetricLens (maptoel key) (eltomap key) smap sel

msgetu :: a (Shared (Map a b)) -> Task (Shared (Map a b),Shared b) | zero b & < a & iTask b & iTask a
msgetu key smap = withShared zero (\el. return (temp el key smap))
/*
msgets :: a (Shared (Map a b)) -> Task (Shared b) | zero b & < a
msgets key smap = withShared zero (\el.( return (snd (symmetricLens (\map el. maybe el id ('DM'.get key map)) (\el map. maybe (map) (\el2. 'DM'.put key el map) ('DM'.get key map)) smap el))))
*/
msgetsm :: a (Shared (Map a b)) -> (Shared (Maybe b)) | < a
msgetsm key smap = mapReadWrite (get,put) smap
    where
        //get :: (Map a b) -> (Maybe b)
        get map = ('DM'.get key map)

        //put :: (Maybe b) (Map a b) -> Maybe (Map a b)
        put mel map = case mel of
                Nothing = Nothing
                Just el = Just ('DM'.put key el map)

msput :: a b (Shared (Map a b)) -> Task (Map a b) | iTask a & iTask b & < a
msput key el smap = upd (\map. 'DM'.put key el map) smap

msputm :: a (Maybe b) (Shared (Map a b)) -> Task (Map a b) | iTask a & iTask b & < a
msputm key mel smap = maybe (get smap) (\el. msput key el smap) mel

msputs :: a (Shared b) (Shared (Map a b)) -> (Shared b, Shared (Map a b)) | iTask a & iTask b & < a & zero b
msputs key sel smap =
	//get sel >>- \el.
	//msput key el smap >>|- //should add the element before asking it
	symmetricLens (eltomap key) (maptoel key) sel smap

msputsm :: a (Shared (Maybe b)) (Shared (Map a b)) -> (Shared (Maybe b), Shared (Map a b)) | iTask a & iTask b & < a
msputsm key smel smap =
    symmetricLens meltomap maptomel smel smap
        where
            //meltomap :: (Maybe b) (Map a b) -> (Map a b)
            meltomap Nothing map = 'DM'.del key map
            meltomap (Just el) map = 'DM'.put key el map

            //maptomel :: (Map a b) (Maybe b) -> (Maybe b)
            maptomel map el = 'DM'.get key map

msset :: a b (Shared (Map a b)) -> Task (Map a b) |  iTask a &  iTask b & < a
msset key el smap = msgetm key smap >>- \mel.
    case mel of
        Nothing = get smap
        Just org = msput key el smap

mssetm :: a (Maybe b) (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a
mssetm key mel smap = maybe (get smap) (\el.msset key el smap) mel

msadd :: a b (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a
msadd key el smap = msgetm key smap >>- \mel.
    case mel of
        Nothing = msput key el smap
        Just org = get smap

msaddm :: a (Maybe b) (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a
msaddm key mel smap = msgetm key smap >>- \morg.
    case morg of
        Nothing = msputm key mel smap
        Just org = get smap

msdel :: a (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a
msdel key smap = upd (\map. 'DM'.del key map) smap

msupd :: a (b -> b) (Shared (Map a b)) -> Task (Map a b) |  iTask a & iTask b & < a
msupd key f smap = msgetm key smap >>- \mel.
    case mel of
        Nothing = get smap
        Just org = msput key (f org) smap

msmapSize :: (Shared (Map a b)) -> Task Int |  iTask a &  iTask b & < a
msmapSize smap = get smap >>- \map. return ('DM'.mapSize map)

msmapSizes :: (Shared (Map a b)) -> (ReadOnlyShared Int) | < a
msmapSizes smap = toReadOnly (mapRead (\map. 'DM'.mapSize map) smap)
