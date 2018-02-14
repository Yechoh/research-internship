definition module dynamicTabs

import iTasks
import qualified Data.Map as DM

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
