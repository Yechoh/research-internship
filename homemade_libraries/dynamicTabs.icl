implementation module dynamicTabs

import iTasks
import qualified Data.Map as DM
import extraTaskCombinators

/**
 * Given a shared Map and a viewable Task over a key and a shared element of that map,
 * shows for every element in the map the corresponding viewtask in a row of tabs,
 * where deleting an element closes the corresponding tab and task and
 * where adding an element opens a corresponding tab and task.
 * The order of the tabs is based on the order of the keys in the Map.
 * It is not possible to put focus on a tab. Which tab is focussed is decided by the user.
 *
 * @param The Shared Map
 * @param A function from Map key to tab name
 * @param The viewable Task. It receives a key and a shared element
 * @result A Task displaying the tabs
 */
dynamicTabs :: (Shared (Map k e)) (k -> String) (k (Shared e) -> Task ()) -> Task [(Int,TaskValue ())] | iTask k & iTask e & < k & zero e
dynamicTabs mapstore totabname t =
	withShared 'DM'.newMap (\tabsstore.
	get mapstore >>- \c.
	parallel [(Embedded,(waitf c tabsstore (updateTabs tabsstore mapstore (ptask tabsstore)))):(map (\k.(Embedded,((ptask tabsstore k)))) ('DM'.keys c))] [] <<@ ApplyLayout (arrangeWithTabs))
	where
	ptask tabsstore key = toParallel t (mapstore >*< tabsstore) totabname key

//takes a task makes it a parallelTask appropiate for use in the dynamic tabs
toParallel :: (k (Shared e) -> Task ()) (Shared ((Map k e),(Map k TaskId))) (k -> String) k -> ParallelTask () | iTask k & iTask e & < k & zero e
toParallel t (maptabsstore) totabname key =
	\tasklist.
	(get (taskListSelfId tasklist) >>- \selfid.
	upd (\(map,tabs).(map,'DM'.put key selfid tabs)) maptabsstore >>|-
	(t key (mapReadWrite (getEl key ,putEl key) maptabsstore))
	>>| upd (\(map,tabs).(('DM'.del key map),('DM'.del key tabs))) maptabsstore >>|- return ())<<@ Title (totabname key)
	where
	getEl :: k ((Map k e),(Map k TaskId)) -> e | < k & zero e
	getEl key (map,tabs) = maybe zero id ('DM'.get key map)

putEl :: k e ((Map k e),(Map k TaskId)) -> Maybe ((Map k e),(Map k TaskId)) | < k
putEl key el (map,tabs) = Just (('DM'.put key el map),tabs)

//wait until the amount of tabs equals the amount of map keys, in other words, wait until all tabs are loaded.
waitf :: (Map k e) (Shared (Map k TaskId)) (ParallelTask ()) -> ParallelTask () | iTask k & iTask e
waitf c tabsstore ptask = \tasklist. watch tabsstore >>* [ OnValue (ifValue (\tabs. sameLengths c tabs) (\tabs. ptask tasklist))]
	where
	sameLengths :: (Map k e) (Map k TaskId) -> Bool
	sameLengths c utabs = ('DM'.mapSize c == 'DM'.mapSize utabs)

//closes tab and task when the corresponding Map element is removed.
//opens tab and task when a Map element is added.
updateTabs :: (Shared (Map k TaskId)) (Shared (Map k e)) (k -> ParallelTask ()) -> ParallelTask () | iTask k & iTask e & < k
updateTabs tabsstore mapstore keyToPtask = \tasklist.
	(watch (mapstore >*< tabsstore))
	>>*
	[	OnValue		(ifValue (\(c,tabs). differentLengths c tabs) (\(c,tabs). (auxa ('DM'.difference c tabs) ('DM'.difference tabs c) tabsstore tasklist keyToPtask) >>|-
			(watch tabsstore)
			 >>*
			 [ 	OnValue (ifValue (\tabs. 'DM'.mapSize tabs == length ('DM'.keys c)) (\tabs. updateTabs tabsstore mapstore keyToPtask tasklist))
			 ]))
	]
	where
	mapTasksSequentially :: (b -> Task ()) [b] -> Task ()
	mapTasksSequentially f l = foldr (\e t. t >>|- f e) (return ()) l

	differentLengths :: (Map k e) (Map k TaskId) -> Bool
	differentLengths c utabs =  not ('DM'.mapSize c == 'DM'.mapSize utabs)

	auxa :: (Map k e) (Map k TaskId) (Shared (Map k TaskId)) (SharedTaskList ()) (k -> ParallelTask()) -> Task () | iTask k & iTask e & <k
	auxa elsNotInTabsstore tabsNotInMapstore tabsstore tasklist keyToPtask =
		aux_append elsNotInTabsstore tabsstore tasklist keyToPtask
		>>|-
		aux_remove tabsNotInMapstore tabsstore tasklist

	aux_append :: (Map k e) (Shared (Map k TaskId)) (SharedTaskList ()) (k -> ParallelTask()) -> Task () | iTask k & <k
	aux_append difference tabsstore tasklist keyToPtask =
		mapTasksSequentially (\key.appendTask Embedded (keyToPtask key) tasklist >>|- return ()) ('DM'.keys difference) >>|-
		return ()

	aux_remove :: (Map k TaskId) (Shared (Map k TaskId)) (SharedTaskList ()) -> Task () | iTask k & <k
	aux_remove difference tabsstore tasklist =
		mapTasksSequentially (\taskid. removeTask taskid tasklist >>|- return () ) ('DM'.elems difference) >>|-
		upd (\tabs. 'DM'.difference tabs difference) tabsstore >>|-
		return ()
