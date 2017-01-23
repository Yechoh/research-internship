definition module targetui

// gui elements for environments

import PmEnvironment
import IdeState
import StdSystem

setProjectTarget :: !String !(PSt *General) -> PSt *General
// sets the environment for the current project

getTargetName :: !(PSt *General) -> (String,PSt *General)
// gets the environment of the current project

targetMenu :: !String [.Target] Id Id (*(PSt *General) -> *([.Target],*(PSt *General))) ([Target] -> .(*(PSt *General) -> *(PSt *General))) -> Menu (:+: .MenuItem (:+: .MenuItem (:+: .MenuItem (:+: .MenuSeparator .RadioMenu)))) .a *(PSt *General);
// define the environments menu

selectProjectTarget :: !(*(PSt *General) -> *([.Target],*(PSt *General))) !*(PSt *General) -> *(PSt *General)
// selects the environment of the current project in the
// environments menu (with availability checking)
