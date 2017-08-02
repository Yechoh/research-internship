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
 * Note that ending task coupled to a key will just reopen it. To really end it, remove the key from the map
 *
 * @param The Shared Map
 * @param A function from Map key to tab name
 * @param The viewable Task. It receives a key and a shared element
 * @param an element nullvalue, needed for consistency, but will never be shown. The nullvalue of int is 0, the nullvalue of list is []
 * @result A Task displaying the tabs
 */
dynamicTabs :: (Shared (Map k e)) (k -> String) (k (Shared e) -> Task ()) e -> Task [(TaskTime,TaskValue ())] | iTask k & iTask e & < k
dynamicTabs mapstore totabname t elnull =
	withShared 'DM'.newMap (\tabsstore.
	get mapstore >>- \c.
	parallel [(Embedded,(waitf c tabsstore (updateTabs tabsstore mapstore (ptask tabsstore)))):(map (\k.(Embedded,((ptask tabsstore k)))) ('DM'.keys c))] [] <<@ ApplyLayout (layoutSubs SelectRoot arrangeWithTabs))
	where
	ptask tabsstore key = toParallel t (mapstore >*< tabsstore) totabname elnull key

//takes a task makes it a parallelTask appropiate for use in the dynamic tabs
toParallel :: (k (Shared e) -> Task ()) (Shared ((Map k e),(Map k TaskId))) (k -> String) e k -> ParallelTask () | iTask k & iTask e & < k
toParallel t (maptabsstore) totabname elnull key =
	\tasklist.
	get (taskListSelfId tasklist) >>- \selfid.
	upd (\(map,tabs).(map,'DM'.put key selfid tabs)) maptabsstore >>|
	(t key (mapReadWrite (mapntabs2elRead key elnull,el2mapntabsWrite key) maptabsstore)) <<@ Title (totabname key)
	>>| upd (\(map,tabs).(('DM'.del key map),('DM'.del key tabs))) maptabsstore >>|- return ()
	where
	mapntabs2elRead :: k e ((Map k e),(Map k TaskId)) -> e | < k
	mapntabs2elRead key elnull (map,tabs) = maybe elnull id ('DM'.get key map)
	
el2mapntabsWrite :: k e ((Map k e),(Map k TaskId)) -> Maybe ((Map k e),(Map k TaskId)) | < k
el2mapntabsWrite key el (map,tabs) = Just (('DM'.put key el map),tabs)

//wait until the amount of tabs equals the amount of map keys, in other words, wait until all tabs are loaded.
waitf :: (Map k e) (Shared (Map k TaskId)) (ParallelTask ()) -> ParallelTask () | iTask k & iTask e
waitf c tabsstore ptask = \tasklist. watch tabsstore >>* [ OnValue (ifValue (\tabs. sameLengths c tabs) (\tabs. ptask tasklist))] 
	where
	sameLengths :: (Map k e) (Map k TaskId) -> Bool
	sameLengths c utabs = ('DM'.mapSize c == 'DM'.mapSize utabs)	
	
//closes tab and task when the corresponding Map element is removed.
//opens tab and task when a Map element is added.
updateTabs :: (Shared (Map k TaskId)) (Shared (Map k e)) (k -> ParallelTask ()) -> ParallelTask () | iTask k & iTask e & < k
updateTabs tabsstore mapstore ptaskf = \tasklist. (viewInformation "" [] "equal" ||- watch (mapstore >*< tabsstore)) 
		>>* [	OnValue		(ifValue (\(c,tabs). differentLengths c tabs) (\(c,tabs). (auxa tabs c tasklist ptaskf) >>|- 
				(viewInformation "" [] "difference" ||- watch tabsstore) >>* [ OnValue (ifValue (\tabs. 'DM'.mapSize tabs == length ('DM'.keys c)) (\tabs. updateTabs tabsstore mapstore ptaskf tasklist))]))]
	where
	mapTasksSequentially :: (b -> Task ()) [b] -> Task ()
	mapTasksSequentially f l = foldr (\e t. t >>|- f e) (return ()) l

	differentLengths :: (Map k e) (Map k TaskId) -> Bool
	differentLengths c utabs =  not ('DM'.mapSize c == 'DM'.mapSize utabs)	
	
	auxa :: (Map k TaskId) (Map k e) (SharedTaskList ()) (k -> ParallelTask ()) -> Task () | iTask k & iTask e & < k
	auxa utabs c tasklist ptaskf = (aux_append utabs c tasklist ptaskf) 
		>>|- (aux_remove utabs c tasklist)
	
	aux_append :: (Map k TaskId) (Map k e) (SharedTaskList ()) (k -> ParallelTask ()) -> Task () | iTask k & iTask e & < k
	aux_append utabs c tasklist ptaskf = mapTasksSequentially (\key.appendTask Embedded (ptaskf key) tasklist >>|- return ()) ('DM'.keys ('DM'.difference c utabs))
	
	aux_remove :: (Map k TaskId) (Map k e) (SharedTaskList ()) -> Task () | iTask k & iTask e & < k
	aux_remove utabs c tasklist = viewInformation "" [] ("deleting",('DM'.elems ('DM'.difference utabs c))) ||- mapTasksSequentially (\taskid. removeTask taskid tasklist) ('DM'.elems ('DM'.difference utabs c)) >>| return ()