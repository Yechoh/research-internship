module test_selectFromTree

import iTasks
import directoryBrowsing

Start world = startEngine (getPwdName >>- \pwd. (selectFromTree True pwd isCleanFile) >>| return ()) world