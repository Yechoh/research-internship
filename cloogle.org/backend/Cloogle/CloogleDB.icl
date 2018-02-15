implementation module CloogleDB

// Standard libraries
import StdArray
import StdBool
import StdFile
from StdFunc import o, const
import StdList
import StdMisc
import StdOrdList
import StdOverloaded
import StdString
import StdTuple

import Control.Applicative
import qualified Control.Monad as CM
import Data.Error
from Data.Func import $
import Data.Functor
from Data.List import intercalate, groupBy, instance Functor []
import qualified Data.Map as DM
import Data.Maybe
import Data.Tuple
import System.File
from Text import class Text(concat), instance Text String
import Text.JSON
import iTasks

import GenLexOrd

// CleanTypeUnifier
import Type

import Cloogle

import Doc

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

printersperse :: Bool a [b] -> [String] | print a & print b
printersperse ia a bs = intercalate (print False a) (map (print ia) bs)

(--) infixr 5 :: a b -> [String] | print a & print b
(--) a b = print False a ++ print False b

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
	//TemporaryDB //choice made to not derive this, since it is not final

/*derive gEq ClassOrGeneric, Location, Type, Priority, FunctionEntry, TypeDef,
	TypeDefRhs, RecordField, Constructor, Kind, Documentation, TypeRhsDoc,
	ClassMemberDoc, InstanceEntry, DeriveEntry
derive JSONEncode ClassOrGeneric, Location, Type, CloogleDB, Priority,
	FunctionEntry, TypeDef, TypeDefRhs, RecordField, Constructor, Kind,
	ModuleEntry, Documentation, TypeRhsDoc, ClassMemberDoc, TypeDefEntry,
	ClassEntry, InstanceEntry, DeriveEntry
derive JSONDecode ClassOrGeneric, Location, Type, CloogleDB, Priority,
	FunctionEntry, TypeDef, TypeDefRhs, RecordField, Constructor, Kind,
	ModuleEntry, Documentation, TypeRhsDoc, ClassMemberDoc, TypeDefEntry,
	ClassEntry, InstanceEntry, DeriveEntry*/

instance zero CloogleDB
where
	zero = { functionmap  = 'DM'.newMap
	       , classmap     = 'DM'.newMap
	       , typemap      = 'DM'.newMap
	       , derivemap    = 'DM'.newMap
	       , modulemap    = 'DM'.newMap
	       , instancemap` = 'DM'.newMap
	       , derivemap`   = 'DM'.newMap
	       }

derive gLexOrd Location, Maybe, ClassOrGeneric, Kind, Type
instance < Location where (<) a b = (a =?= b) === LT
instance < (Maybe a) | gLexOrd{|*|} a where (<) a b = (a =?= b) === LT
instance < Type where (<) a b = (a =?= b) === LT
instance < (a,b,c,d) | gLexOrd{|*|} a & gLexOrd{|*|} b & gLexOrd{|*|} c & gLexOrd{|*|} d
	where (<) a b = (a =?= b) === LT

instance == Location where == a b = gEq{|*|} a b
instance == DeriveEntry where == a b = gEq{|*|} a b


instance zero FunctionEntry
where
	zero = { fe_kind           = Function
	       , fe_type           = Nothing
	       , fe_priority       = Nothing
	       , fe_generic_vars   = Nothing
	       , fe_representation = Nothing
	       , fe_documentation  = Nothing
	       }

instance zero ModuleEntry
where zero = {me_is_core = False, me_documentation=Nothing}

instance print (Name, FunctionEntry)
where
	print b (f, fe)
		= gen -- fname -- " " -- prio -- vars -- if (isJust fe.fe_type) (":: " -- fe.fe_type) []
	where
		prio = case fe.fe_priority of
			Nothing -> []
			Just p -> print b p -- " "
		vars = case fe.fe_generic_vars of
			Nothing -> []
			Just vs -> printersperse b " " vars -- " "
		gen = if (isJust fe.fe_generic_vars) "generic " ""
		fname
		| isJust fe.fe_priority     = concat ("(" -- f -- ")")
		| fe.fe_kind == RecordField = "." +++ f
		| otherwise                 = f

getName :: Location -> Name
getName (Location _ _ _ _ name) = name
getName (Builtin name)          = name

isBuiltin :: Location -> Bool
isBuiltin (Builtin _) = True
isBuiltin _           = False

isCore :: Location CloogleDB -> Bool
isCore (Builtin _) _ = False
isCore (Location lib mod _ _ _) db = case getModule lib mod db of
	Nothing  = False
	(Just b) = b.me_is_core

toTypeDefEntry :: TypeDef (Maybe Documentation) -> TypeDefEntry
toTypeDefEntry td doc = {tde_typedef=td, tde_doc=doc}

getTypeDef :: TypeDefEntry -> TypeDef
getTypeDef {tde_typedef} = tde_typedef

getTypeDefDoc :: TypeDefEntry -> Maybe Documentation
getTypeDefDoc {tde_doc} = tde_doc

toClass :: [String] ClassContext (Maybe Documentation) [(Name, FunctionEntry)] -> ClassEntry
toClass vs cc doc mems
	= { ce_vars = vs
	  , ce_context = cc
	  , ce_documentation = doc
	  , ce_members = mems
	  , ce_instances = []
	  }

functionCount :: CloogleDB -> Int
functionCount {functionmap} = 'DM'.mapSize functionmap

classCount :: CloogleDB -> Int
classCount {classmap} = 'DM'.mapSize classmap

typeCount :: CloogleDB -> Int
typeCount {typemap} = 'DM'.mapSize typemap

deriveCount :: CloogleDB -> Int
deriveCount {derivemap} = sum $ map length $ 'DM'.elems derivemap

moduleCount :: CloogleDB -> Int
moduleCount {modulemap} = 'DM'.mapSize modulemap

filterLocations :: (Location -> Bool) CloogleDB -> CloogleDB
filterLocations f db
	= { db
	  & functionmap = filterLoc f db.functionmap
	  , classmap    = filterLoc f db.classmap
	  , typemap     = filterLoc f db.typemap
	  , derivemap   = 'DM'.mapWithKey (\k a. filtDervLocs f a) db.derivemap
	  , modulemap   = filtModules f db.modulemap
	  }

filterLoc :: (Location -> Bool) (Map Location a) -> Map Location a
filterLoc f m = 'DM'.filterWithKey (const o f) m

filtDervLocs :: (Location -> Bool) [DeriveEntry] -> [DeriveEntry]
filtDervLocs f [] = []
filtDervLocs f [de:rest] = case filter f de.de_locations of
	[] = filtDervLocs f rest
	ls = [{de & de_locations=ls}:filtDervLocs f rest]

filtModules :: (Location -> Bool) (Map (Library, Module) a) -> Map (Library, Module) a
filtModules f m = 'DM'.filterWithKey (\(l,m) _ -> f (Location l m Nothing Nothing "")) m

getFunction :: !Location !CloogleDB -> Maybe FunctionEntry
getFunction loc db = 'DM'.get loc db.functionmap

putFunction :: !Location !FunctionEntry !CloogleDB -> CloogleDB
putFunction fl t db = { db & functionmap = 'DM'.put fl t db.functionmap }

putFunctions :: ![(!Location, !FunctionEntry)] !CloogleDB -> CloogleDB
putFunctions ts db = foldr (uncurry putFunction) db ts

findFunction :: !Name !CloogleDB -> [(!Location, !FunctionEntry)]
findFunction f db = filter ((==) f o getName o fst) $ 'DM'.toList db.functionmap

findFunction` :: !(Location FunctionEntry -> Bool) !CloogleDB
	-> [(!Location, !FunctionEntry)]
findFunction` f db = filter (uncurry f) $ 'DM'.toList db.functionmap

findFunction`` :: ![(Location FunctionEntry -> Bool)] !CloogleDB
	-> [(!Location, !FunctionEntry)]
findFunction`` fs db = foldr (filter o uncurry) ('DM'.toList db.functionmap) fs

getInstances :: !Name !CloogleDB -> [InstanceEntry]
getInstances c db = [ie \\ (_,ce) <- findClass c db, ie <- ce.ce_instances]

putInstance :: !Name ![(!Type,!String)] !Location !CloogleDB -> CloogleDB
putInstance c t l db
	= {db & classmap='DM'.mapWithKey (\loc ce -> if (getName loc == c)
		{ce & ce_instances=update ce.ce_instances} ce) db.classmap}
where
	update :: [InstanceEntry] -> [InstanceEntry]
	update []   = [{ie_types=t,ie_locations=[l]}]
	update [ie:rest]
	| ie.ie_types == t = [{ie & ie_locations=removeDup [l:ie.ie_locations]}:rest]
	| otherwise        = [ie:update rest]

putInstances :: ![(!Name, ![(!Type,!String)], !Location)] !CloogleDB -> CloogleDB
putInstances is db = foldr (\(c,ts,l) -> putInstance c ts l) db is

getClass :: !Location !CloogleDB -> Maybe ClassEntry
getClass loc db = 'DM'.get loc db.classmap

putClass :: !Location !ClassEntry !CloogleDB -> CloogleDB
putClass cl def db = {db & classmap='DM'.put cl def db.classmap}

putClasses :: ![(!Location, !ClassEntry)] !CloogleDB -> CloogleDB
putClasses cs db = foldr (uncurry putClass) db cs

findClass :: !Name !CloogleDB -> [(!Location, !ClassEntry)]
findClass c db = filter ((==) c o getName o fst) $ 'DM'.toList db.classmap

findClass` :: !(Location ClassEntry -> Bool) !CloogleDB -> [(!Location, !ClassEntry)]
findClass` f db = filter (uncurry f) $ 'DM'.toList db.classmap

findClass`` :: ![(Location ClassEntry -> Bool)] !CloogleDB -> [(!Location, !ClassEntry)]
findClass`` fs db = foldr (filter o uncurry) ('DM'.toList db.classmap) fs

findClassMembers` :: !(Location [String] ClassContext Name FunctionEntry -> Bool) !CloogleDB
		-> [(!Location, ![String], !ClassContext, !Name, !FunctionEntry)]
findClassMembers` f db = filter (app5 f)
	[(cl,def.ce_vars,def.ce_context,f,t)
		\\ (cl,def) <- 'DM'.toList db.classmap, (f,t) <- def.ce_members]

findClassMembers`` :: ![(Location [String] ClassContext Name FunctionEntry -> Bool)]
		!CloogleDB -> [(!Location, ![String], !ClassContext, !Name, !FunctionEntry)]
findClassMembers`` fs db = foldr (filter o app5) all_members fs
where
	all_members = [(cl,def.ce_vars,def.ce_context,f,t)
		\\ (cl,def) <- 'DM'.toList db.classmap, (f,t) <- def.ce_members]

getType :: !Location !CloogleDB -> Maybe TypeDefEntry
getType loc db = 'DM'.get loc db.typemap

putType :: !Location !TypeDefEntry !CloogleDB -> CloogleDB
putType tl td db = {db & typemap='DM'.put tl td db.typemap}

putTypes :: ![(!Location, !TypeDefEntry)] !CloogleDB -> CloogleDB
putTypes ts db = foldr (uncurry putType) db ts

findType :: !Name !CloogleDB -> [(!Location, !TypeDefEntry)]
findType t db = filter ((==) t o getName o fst) $ 'DM'.toList db.typemap

findType` :: !(Location TypeDefEntry -> Bool) !CloogleDB
		-> [(!Location, !TypeDefEntry)]
findType` f db = filter (uncurry f) $ 'DM'.toList db.typemap

findType`` :: ![(Location TypeDefEntry -> Bool)] !CloogleDB
		-> [(!Location, !TypeDefEntry)]
findType`` fs db = foldr (filter o uncurry) ('DM'.toList db.typemap) fs

allTypes :: (CloogleDB -> [TypeDefEntry])
allTypes = map snd o findType` (\_ _ -> True)

getDerivations :: !Name !CloogleDB -> [DeriveEntry]
getDerivations gen db = mb2list ('DM'.get gen db.derivemap)

putDerivation :: !Name !Type !String !Location !CloogleDB -> CloogleDB
putDerivation gen t s loc db = {db & derivemap='DM'.put gen ts db.derivemap}
where ts = removeDup [{de_type=t, de_type_representation=s, de_locations=[loc]} : getDerivations gen db]

putDerivations :: !Name ![(!Type, !String, !Location)] !CloogleDB -> CloogleDB
putDerivations gen ts db = foldr (\(t,s,l) db -> putDerivation gen t s l db) db ts

putDerivationss :: ![(!Name, ![(!Type, !String, !Location)])] !CloogleDB -> CloogleDB
putDerivationss ds db = foldr (uncurry putDerivations) db ds

getTypeInstances :: !Name !CloogleDB -> [(!Name, !InstanceEntry)]
getTypeInstances n db = mb2list $ 'DM'.get n db.instancemap`

getTypeDerivations :: !Name !CloogleDB -> [(!Name, ![Location])]
getTypeDerivations n db = mb2list $ 'DM'.get n db.derivemap`

getModule :: !Library !Module !CloogleDB -> Maybe ModuleEntry
getModule lib mod db = 'DM'.get (lib,mod) db.modulemap

putModule :: !Library !Module !ModuleEntry !CloogleDB -> CloogleDB
putModule lib mod info db = {db & modulemap='DM'.put (lib,mod) info db.modulemap}

findModule` :: !(Library Module ModuleEntry -> Bool) !CloogleDB -> [(!Library, !Module, !ModuleEntry)]
findModule` f db = map (\((l,m),i) -> (l,m,i)) $ filter (uncurry $ uncurry f) $ 'DM'.toList db.modulemap

newDb :: CloogleDB
newDb = zero

syncDb :: !CloogleDB -> CloogleDB
syncDb db
	= { db
	  & instancemap` = insts
	  , derivemap`   = derivs
	  }
where
	insts = 'DM'.fromList $ map (\cs=:[(t,_):_] -> (t,map snd cs)) $
		groupBy (\a b -> fst a == fst b) $ sortBy (\a b -> fst a < fst b)
		[(t,(getName c,ie)) \\ (c,ce) <- 'DM'.toList db.classmap, ie <- ce.ce_instances, (Type t _,_) <- ie.ie_types]
	derivs = 'DM'.fromList $ map (\gs=:[(t,_,_):_] -> (t,[(g,ls) \\ (_,g,ls) <- gs])) $
		groupBy (\a b -> fst3 a == fst3 b) $ sort
		[(t,g,de.de_locations) \\ (g,des) <- 'DM'.toList db.derivemap, de=:{de_type=Type t []} <- des]

openDb :: !*File -> *(!CloogleDB, !*File)
openDb f
#! (data,f) = freadline f
#! (Just db) = fromJSON $ fromString data
= (db, f)

saveDb :: !CloogleDB !*File -> *File
saveDb db f = f <<< toJSON db

app5 f (a,b,c,d,e) :== f a b c d e
mb2list m :== case m of Nothing -> []; Just xs -> xs
