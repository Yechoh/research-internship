definition module PmDirCache

from StdMaybe			import Maybe
from StdFile			import Files
//1.3
from StdString import String
//3.1
from UtilStrictLists	import List
from PmTypes			import Modulename
from StdPathname		import Pathname
from Directory			import DateTime,Date`,Time`

:: DirCache

:: Warn = Warn String String [(String,String,DateTime)]

DC_Setup	:: !(List Pathname) !*Files -> (!(![String],![Warn],!.DirCache),!*Files)
// Initialise directory cache

DC_Search	:: !Modulename !*DirCache -> !*(!Bool,!Pathname,!DateTime,!*DirCache)
// Find file in directory cache

DC_Update	:: !.(String,String,DateTime) !*DirCache -> !*DirCache
// Update directory cache

SearchDisk	:: !Bool !Modulename !(List Pathname) !*Files -> ((!Bool,!Pathname),!*Files)
// Find file in paths (no caching)
