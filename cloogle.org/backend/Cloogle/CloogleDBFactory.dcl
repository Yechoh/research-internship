definition module CloogleDBFactory

/**
 * Functions to populate a database using the Clean compiler frontend
 */

import CloogleDB

:: TemporaryDB

newTemporaryDb :: TemporaryDB
finaliseDb :: !TemporaryDB !CloogleDB -> CloogleDB

/**
 * Find all modules that could be indexed
 *
 * @param Excluded modules. If the path contains any of the strings in this
 *   parameter, a module will not be considered.
 * @param The root of the library directory (typically $CLEAN_HOME/lib).
 * @param The library to look in.
 * @param A function indicating if a module is part of the library core.
 * @param A part of the module hierarchy to look for. The empty string to look
 *   in the whole library, otherwise e.g. Crypto.Hash to only include modules
 *   in that hierarchy.
 * @param The World.
 * @result A list of modules found (library, module and whether it is part of
 *   the library core).
 */
findModules :: ![String] !String !Library (Module -> Bool) !String !*World
	-> *(![(Library, Module, Bool)], !*World)

/**
 * Update a database with all the information found in a module
 *
 * @param The root of the library directory (typically $CLEAN_HOME/lib).
 * @param The module to index.
 * @param The library the module to index is in.
 * @param Whether this is a core module.
 * @param The old database.
 * @result The new database.
 */
indexModule :: !String !Module !Library !Bool !TemporaryDB !*World
	-> *(!TemporaryDB, !*World)

/**
 * Transform the constructors of an algebraic data type into plain functions.
 */
constructor_functions :: (Location, TypeDefEntry) -> [(Location, FunctionEntry)]

/**
 * Transform the record fields of a record type into plain functions.
 */
record_functions :: (Location, TypeDefEntry) -> [(Location, FunctionEntry)]
