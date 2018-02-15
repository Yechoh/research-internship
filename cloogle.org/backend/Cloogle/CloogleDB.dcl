definition module CloogleDB

import Type
import StdArray
import Doc
/**
 * A database with information about Clean modules.
 *
 * The functions here follow a general guideline about nomenclature:
 * - get* expects that you already know the location of the value you seek and
 *   will only return an exact result, if it can be found. This runs in
 *   O(log n) where n is the number of elements of that type in the database.
 * - find* searches by name, but can return results from multiple libraries.
 *   This is O(n).
 * - find*` searches using a property function, similar to StdEnv's filter.
 *   This is O(n).
 * - find*`` accepts a list of property functions that all have to match for
 *   the entry to be included in the result. It essentially folds the
 *   properties on the database with the single-backtick version. This is O(n).
 * - put* adds a single entry to the database.
 * - put*s adds a number of entries to the database.
 */


// Standard libraries
from StdOverloaded import class <, class zero
from StdClass import class Ord

from Data.Map import :: Map
from Data.Maybe import :: Maybe

from GenEq import generic gEq

:: CloogleDB
	= { // Base maps
	    functionmap  :: Map Location FunctionEntry
	  , classmap     :: Map Location ClassEntry
	  , typemap      :: Map Location TypeDefEntry
	  , derivemap    :: Map Name [DeriveEntry]
	  , modulemap    :: Map (Library, Module) ModuleEntry
	    // Derived maps
	  , instancemap` :: Map Name [(Name, InstanceEntry)]
	  , derivemap`   :: Map Name [(Name, [Location])]
	  }

// CleanTypeUnifier

from Cloogle import :: FunctionKind

import iTasks

derive class iTask
   CloogleDB,
   Location,
   FunctionEntry,
   Type,
   ClassOrGeneric,
   Kind,
   Priority,
   Documentation,
   ClassMemberDoc,
   TypeRhsDoc,
   FunctionKind,
   ClassEntry,
   TypeDefEntry,
   DeriveEntry,
   ModuleEntry,
   InstanceEntry,
   TypeDef,
   TypeDefRhs,
   Constructor,
   RecordField

/**
 * A storage for function types, class definitions, type definitions, etc.
 */
//:: CloogleDB

/**
 * A location in the Clean libraries
 */
:: Location
	= Location !Library !Module !LineNr !LineNr !Name //* A normal location
	| Builtin                                   !Name //* A language builtin
	| Project  !String 	!Module !LineNr !LineNr !Name

/**
 * Not-type information that is often associated with things that have a type
 */
:: FunctionEntry
	= { fe_kind           :: FunctionKind   //* The type of entry
	  , fe_type           :: Maybe Type     //* The type, Nothing for macros
	  , fe_priority       :: Maybe Priority //* The infix priority
	  , fe_generic_vars   :: Maybe [String] //* The names of the type variables of a generic function
	    // Using TypeVar causes import clashes in CloogleDBFactory
	  , fe_representation :: Maybe String   //* A string representation of the entry
	  , fe_documentation  :: Maybe Documentation //* Documentation on this entry
	  }

/**
 * A TypeDef with meta-data
 */
:: TypeDefEntry
	= { tde_typedef :: !TypeDef             //* The TypeDef
	  , tde_doc     :: !Maybe Documentation //* Documentation on the TypeDef (a TypeDoc)
	  }

/**
 * Information about a Clean module
 */
:: ModuleEntry
	= { me_is_core       :: !Bool                //* Whether this is a core module (e.g. the os* modules in ObjectIO and TCPIP)
	  , me_documentation :: !Maybe Documentation //* Documentation on this module (a ModuleDoc)
	  }

/**
 * Information about a Clean class
 */
:: ClassEntry
	= { ce_vars          :: ![String]                  //* The type variables of the class
	    // Using TypeVar causes import clashes in CloogleDBFactory
	  , ce_context       :: !ClassContext              //* A class context
	  , ce_documentation :: !Maybe Documentation       //* Documentation on this class (a ClassDoc)
	  , ce_members       :: ![(!Name, !FunctionEntry)] //* Class members: function name and type information
	  , ce_instances     :: ![InstanceEntry]           //* All instances of the class
	  }

/**
 * Information about a class instance
 */
:: InstanceEntry
	= { ie_types     :: [(Type, String)] //* The instantiated type and a string representation for each class variable
	  , ie_locations :: [Location]       //* The places where this instance is found
	  }

/**
 * Information about a generic derivation
 */
:: DeriveEntry
	= { de_type                :: Type       //* The type to derive an instance for
	  , de_type_representation :: String     //* A string representation of the type
	  , de_locations           :: [Location] //* The locations in which the derivation occurs
	  }

:: Name    :== String
:: Library :== String
:: Module  :== String
:: LineNr  :== Maybe Int

instance zero CloogleDB
instance zero FunctionEntry
instance zero ModuleEntry

instance print (Name, FunctionEntry)

getName :: Location -> Name
isBuiltin :: Location -> Bool
isCore :: Location CloogleDB -> Bool

toTypeDefEntry :: TypeDef (Maybe Documentation) -> TypeDefEntry
getTypeDef :: TypeDefEntry -> TypeDef
getTypeDefDoc :: TypeDefEntry -> Maybe Documentation

/**
 * Wrapper around the Class record field to work around name clashes
 *
 * @param The type variables of the class
 * @param The class context
 * @param The documentation (a ClassDoc)
 * @param The names and types of the class members
 * @result A Class record with those data
 */
toClass :: [String] ClassContext (Maybe Documentation) [(Name, FunctionEntry)] -> ClassEntry

functionCount :: CloogleDB -> Int
classCount :: CloogleDB -> Int
typeCount :: CloogleDB -> Int
deriveCount :: CloogleDB -> Int
moduleCount :: CloogleDB -> Int

/**
 * Filter the whole database on locations using a property function
 */
filterLocations :: (Location -> Bool) CloogleDB -> CloogleDB

getFunction :: !Location !CloogleDB -> Maybe FunctionEntry
putFunction :: !Location !FunctionEntry !CloogleDB -> CloogleDB
putFunctions :: ![(!Location, !FunctionEntry)] !CloogleDB -> CloogleDB
findFunction :: !Name !CloogleDB -> [(!Location, !FunctionEntry)]
findFunction` :: !(Location FunctionEntry -> Bool) !CloogleDB
		-> [(!Location, !FunctionEntry)]
findFunction`` :: ![(Location FunctionEntry -> Bool)] !CloogleDB
		-> [(!Location, !FunctionEntry)]

getInstances :: !Name !CloogleDB -> [InstanceEntry]
putInstance :: !Name ![(!Type,!String)] !Location !CloogleDB -> CloogleDB
putInstances :: ![(!Name, ![(!Type,!String)], !Location)] !CloogleDB -> CloogleDB

getClass :: !Location !CloogleDB -> Maybe ClassEntry
putClass :: !Location !ClassEntry !CloogleDB -> CloogleDB
putClasses :: ![(!Location, !ClassEntry)] !CloogleDB -> CloogleDB
findClass :: !Name !CloogleDB -> [(!Location, !ClassEntry)]
findClass` :: !(Location ClassEntry -> Bool) !CloogleDB -> [(!Location, !ClassEntry)]
findClass`` :: ![(Location ClassEntry -> Bool)] !CloogleDB -> [(!Location, !ClassEntry)]

findClassMembers` :: !(Location [String] ClassContext Name FunctionEntry -> Bool) !CloogleDB
		-> [(!Location, ![String], !ClassContext, !Name, !FunctionEntry)]
findClassMembers`` :: ![(Location [String] ClassContext Name FunctionEntry -> Bool)]
		!CloogleDB -> [(!Location, ![String], !ClassContext, !Name, !FunctionEntry)]

getType :: !Location !CloogleDB -> Maybe TypeDefEntry
putType :: !Location !TypeDefEntry !CloogleDB -> CloogleDB
putTypes :: ![(!Location, !TypeDefEntry)] !CloogleDB -> CloogleDB
findType :: !Name !CloogleDB -> [(!Location, !TypeDefEntry)]
findType` :: !(Location TypeDefEntry -> Bool) !CloogleDB -> [(!Location, !TypeDefEntry)]
findType`` :: ![(Location TypeDefEntry -> Bool)] !CloogleDB -> [(!Location, !TypeDefEntry)]
allTypes :: (CloogleDB -> [TypeDefEntry])

getDerivations :: !Name !CloogleDB -> [DeriveEntry]
putDerivation :: !Name !Type !String !Location !CloogleDB -> CloogleDB
putDerivations :: !Name ![(!Type, !String, !Location)] !CloogleDB -> CloogleDB
putDerivationss :: ![(!Name, ![(!Type, !String, !Location)])] !CloogleDB -> CloogleDB

getModule :: !Library !Module !CloogleDB -> Maybe ModuleEntry
putModule :: !Library !Module !ModuleEntry !CloogleDB -> CloogleDB
findModule` :: !(Library Module ModuleEntry -> Bool) !CloogleDB -> [(!Library, !Module, !ModuleEntry)]

/**
 * Get all the class instances of a type
 *
 * @param The name of the type
 * @param The database
 * @result A list of class instances (name of the class, the full list of types
 *   instantiating the class (type and string representation) and all the
 *   locations where the class is instantiated for those types)
 */
getTypeInstances :: !Name !CloogleDB -> [(!Name, !InstanceEntry)]

/**
 * Get all the generic derivations of a type
 *
 * @param The name of the type
 * @param The database
 * @result A list of derivations (name of the generic function and all the
 *   locations where it is derived for that type)
 */
getTypeDerivations :: !Name !CloogleDB -> [(!Name, ![Location])]

/**
 * Initialise an empty database
 */
newDb :: CloogleDB

/**
 * Synchronise the database. Should be called after updating data, to update
 * derived information.
 */
syncDb :: !CloogleDB -> CloogleDB

/**
 * Read the database from a file. The file should be opened for reading.
 */
openDb :: !*File -> *(!CloogleDB, !*File)

/**
 * Save the database to a file. The file should be opened for writing.
 */
saveDb :: !CloogleDB !*File -> *File
