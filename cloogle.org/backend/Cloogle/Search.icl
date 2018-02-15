implementation module Search

import StdArray
import StdBool
from StdFunc import const, id, flip, o
import StdList
import StdOrdList
import StdString
import StdTuple

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as Foldable
from Data.Foldable import class Foldable, instance Foldable Maybe
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
from Text import class Text(concat,indexOf,toLowerCase,split),
	instance Text String, instance + String

import CloogleDB
import Type
import Cloogle
import Doc

search :: !Request !CloogleDB -> [Result]
search {unify,name,className,typeName,modules,libraries,page,include_builtins,include_core} db_org
	# include_builtins = fromJust (include_builtins <|> Just DEFAULT_INCLUDE_BUILTINS)
	# include_core = fromJust (include_core <|> Just DEFAULT_INCLUDE_CORE)
	# db = db_org
	# db = case libraries of
		(Just ls) = filterLocations (isLibMatch ls) db
		Nothing   = db
	# db = case modules of
		(Just ms) = filterLocations (isModMatch ms) db
		Nothing   = db
	# db = if include_builtins id (filterLocations (not o isBuiltin)) db
	# db = if include_core id (filterLocations (not o flip isCore db)) db
	| isJust className
		# className = fromJust className
		# classes = findClass className db
		= map (flip makeClassResult db) classes
	| isJust typeName
		# typeName = fromJust typeName
		# types = findType typeName db
		= [makeTypeResult (Just typeName) l td db \\ (l,td) <- types]
	# mbPreppedType = prepare_unification True (map getTypeDef $ allTypes db_org)
		<$> (unify >>= parseType o fromString)
	# usedSynonyms = 'Foldable'.concat (fst <$> mbPreppedType)
	# mbType = snd <$> mbPreppedType
	// Search normal functions
	# filts = catMaybes [ (\t _ -> isUnifiable t db_org) <$> mbType
	                    , (\n loc _ -> isNameMatch (size n*2/3) n $ getName loc) <$> name
	                    ]
	# funs = map (\f -> makeFunctionResult name mbType usedSynonyms Nothing f db_org) $ findFunction`` filts db
	// Search class members
	# filts = catMaybes [ (\t _ _ _ _ -> isUnifiable t db_org) <$> mbType
	                    , (\n (Location lib mod _ _ _) _ _ f _ -> isNameMatch
	                      (size n*2/3) n f) <$> name
	                    ]
	# members = findClassMembers`` filts db
	# members = map (\(Location lib mod line iclline cls,vs,_,f,et) -> makeFunctionResult name mbType usedSynonyms
		(Just {cls_name=cls,cls_vars=vs}) (Location lib mod line iclline f,et) db) members
	// Search types
	# lcName = if (isJust mbType && isType (fromJust mbType))
		(let (Type name _) = fromJust mbType in Just $ toLowerCase name)
		(toLowerCase <$> name)
	# types = case (mbType,lcName) of
		(Nothing, Just n)   = findType` (\loc _ -> toLowerCase (getName loc) == n) db
		(Just (Type n _),_) = findType n db
		(Just _, _)         = []
	# types = map (\(tl,td) -> makeTypeResult name tl td db) types
	// Search classes
	# classes = case (isNothing mbType, toLowerCase <$> name) of
		(True, Just c) = findClass` (\loc _ -> toLowerCase (getName loc) == c) db
		_              = []
	# classes = map (flip makeClassResult db) classes
	// Search modules
	# modules = case (mbType, name) of
		(Nothing, Just n) = findModule` (\_ m _ -> isModNameMatch (size n*2/3) n m) db
		_                 = []
	# modules = map (makeModuleResult name) modules
	// Merge results
	= sort $ funs ++ members ++ types ++ classes ++ modules

makeModuleResult :: (Maybe String) (Library, Module, ModuleEntry) -> Result
makeModuleResult mbName (lib, mod, info)
	= ModuleResult
	  ( { library       = lib
	    , modul         = mod
	    , filename      = modToFilename mod
	    , dcl_line      = Nothing
	    , icl_line      = Nothing
	    , distance      = modLevenshtein (fromJust mbName) mod
	    , builtin       = Nothing
	    , documentation = getDocDescription =<< info.me_documentation
	    }
	  , { module_is_core = info.me_is_core
	    }
	  )

makeClassResult :: (Location, ClassEntry) CloogleDB -> Result
makeClassResult rec=:(Builtin _, def) db
	= ClassResult
	  ( { library       = ""
	    , filename      = ""
	    , dcl_line      = Nothing
	    , icl_line      = Nothing
	    , modul         = ""
	    , distance      = -100
	    , builtin       = Just True
	    , documentation = getDocDescription =<< def.ce_documentation
	    }
	  , makeClassResultExtras rec db
	  )
makeClassResult rec=:(Location lib mod line iclline cls, def) db
	= ClassResult
	  ( { library       = lib
	    , filename      = modToFilename mod
	    , dcl_line      = line
	    , icl_line      = iclline
	    , modul         = mod
	    , distance      = -100
	    , builtin       = Nothing
	    , documentation = getDocDescription =<< def.ce_documentation
	    }
	  , makeClassResultExtras rec db
	  )
makeClassResultExtras :: (Location, ClassEntry) CloogleDB -> ClassResultExtras
makeClassResultExtras (l, def) db
	= { class_name = cls
	  , class_heading = foldl ((+) o (flip (+) " ")) cls def.ce_vars +
	      if (isEmpty def.ce_context) "" " | " + concat (print False def.ce_context)
	  , class_funs = [print_fun fun \\ fun <- def.ce_members]
	  , class_instances
	      = sortBy (\(a,_) (b,_) -> a < b)
	          [(map snd ie.ie_types, map loc ie.ie_locations) \\ ie <- getInstances cls db]
	  }
where
	cls = getName l

	print_fun :: (Name,FunctionEntry) -> String
	print_fun f=:(_,fe) = fromJust $
		fe.fe_representation <|> (pure $ concat $ print False f)

makeTypeResult :: (Maybe String) Location TypeDefEntry CloogleDB -> Result
makeTypeResult mbName (Location lib mod line iclline t) etd db
	= TypeResult
	  ( { library       = lib
	    , filename      = modToFilename mod
	    , dcl_line      = line
	    , icl_line      = iclline
	    , modul         = mod
	    , distance
	        = if (isNothing mbName) -100 (levenshtein` t (fromJust mbName))
	    , builtin       = Nothing
	    , documentation = getDocDescription =<< etd.tde_doc
	    }
	  , { type                    = concat $ print False etd.tde_typedef
	    , type_instances          = [(c, map snd ie.ie_types, map loc ie.ie_locations) \\ (c,ie) <- getTypeInstances t db]
	    , type_derivations        = map (appSnd (map loc)) $ getTypeDerivations t db
	    , type_field_doc          = getFieldDoc =<< etd.tde_doc
	    , type_constructor_doc    = map ((=<<) getDocDescription) <$> (getConstructorDoc =<< etd.tde_doc)
	    , type_representation_doc = getRepresentationDoc =<< etd.tde_doc
	    }
	  )
makeTypeResult mbName (Builtin t) etd db
	= TypeResult
	  ( { library       = ""
	    , filename      = ""
	    , dcl_line      = Nothing
	    , icl_line      = Nothing
	    , modul         = ""
	    , distance
	        = if (isNothing mbName) -100 (levenshtein` t (fromJust mbName))
	    , builtin       = Just True
	    , documentation = getDocDescription =<< etd.tde_doc
	    }
	  , { type                    = concat $ print False etd.tde_typedef
	    , type_instances          = [(c, map snd ie.ie_types, map loc ie.ie_locations) \\ (c,ie) <- getTypeInstances t db]
	    , type_derivations        = map (appSnd (map loc)) $ getTypeDerivations t db
	    , type_field_doc          = getFieldDoc =<< etd.tde_doc
	    , type_constructor_doc    = map ((=<<) getDocDescription) <$> (getConstructorDoc =<< etd.tde_doc)
	    , type_representation_doc = getRepresentationDoc =<< etd.tde_doc
	    }
	  )

makeFunctionResult :: (Maybe String) (Maybe Type) [TypeDef] (Maybe ShortClassResult)
	(Location, FunctionEntry) CloogleDB -> Result
makeFunctionResult orgsearch orgsearchtype usedsynonyms mbCls (fl, fe) db
	# type = prepare_unification` False db <$> fe.fe_type
	# unif = type >>= \(syns,type) ->
		finish_unification (syns ++ usedsynonyms) <$>
		(orgsearchtype >>= unify [] type)
	= FunctionResult
	  ( { library       = lib
	    , filename      = modToFilename mod
	    , dcl_line      = line
	    , icl_line      = iclline
	    , modul         = mod
	    , distance      = distance unif
	    , builtin       = builtin
	    , documentation = getDocDescription =<< fe.fe_documentation
	    }
	  , { kind     = fe.fe_kind
	    , func     = fromJust (fe.fe_representation <|>
	                           (pure $ concat $ print False (fname,fe)))
	    , unifier  = toStrUnifier <$> unif
	    , cls      = mbCls
	    , constructor_of = if (fe.fe_kind == Constructor)
	        (let (Just (Func _ r _)) = fe.fe_type in Just $ concat $ print False r)
	        Nothing
	    , recordfield_of = if (fe.fe_kind == RecordField)
	        (let (Just (Func [t:_] _ _)) = fe.fe_type in Just $ concat $ print False t)
	        Nothing
	    , generic_derivations
	        = let derivs = getDerivations fname db in
	          const (sortBy (\(a,_) (b,_) -> a < b)
	              [(de.de_type_representation, map loc de.de_locations) \\ de <- derivs]) <$>
	          fe.fe_generic_vars
	    , param_doc = getParamDocs <$> fe.fe_documentation
	    , generic_var_doc = getVarDocs <$> fe.fe_documentation
	    , result_doc = getResultDoc =<< fe.fe_documentation
	    }
	  )
where
	(lib,mod,fname,line,iclline,builtin) = case fl of
		(Location l m ln iln f) = (l,  m,  f, ln,      iln,     Nothing)
		(Builtin f)             = ("", "", f, Nothing, Nothing, Just True)

	toStrUnifier :: Unifier -> StrUnifier
	toStrUnifier unif =
		{ StrUnifier
		| left_to_right = map toStr unif.Unifier.left_to_right
		, right_to_left = map toStr unif.Unifier.right_to_left
		, used_synonyms = [
			( concat $ [td.td_name," ":intersperse " " $ print False td.td_args]
			, concat $ print False s)
			\\ td=:{td_rhs=TDRSynonym s} <- unif.Unifier.used_synonyms]
		}
	where
		toStr (var, type) = (var, concat $ print False type)

	toStrPriority :: (Maybe Priority) -> String
	toStrPriority p = case print False p of [] = ""; ss = concat [" ":ss]

	distance unif
		| isNothing orgsearch || fromJust orgsearch == ""
			| isNothing orgsearchtype = 0
			# orgsearchtype = fromJust orgsearchtype
			= penalty + toInt (sum [typeComplexity t \\ (_,t) <- allTvas $ fromJust unif | not (isVar t)])
		# orgsearch = fromJust orgsearch
		= penalty + levenshtein` orgsearch fname
	where
		penalty = case fe.fe_kind of
			RecordField -> 2
			Constructor -> 1
			_           -> 0

		allTvas :: Unifier -> [TVAssignment]
		allTvas unif = unif.Unifier.left_to_right ++ unif.Unifier.right_to_left

		typeComplexity :: Type -> Real
		typeComplexity (Type _ ts) = 1.2 * foldr ((+) o typeComplexity) 1.0 ts
		typeComplexity (Func is r _) = 2.0 * foldr ((+) o typeComplexity) 1.0 [r:is]
		typeComplexity (Var _) = 1.0
		typeComplexity (Cons _ ts) = 1.2 * foldr ((+) o typeComplexity) 1.0 ts
		typeComplexity (Uniq t) = 3.0 + typeComplexity t

prepare_unification` :: !Bool !CloogleDB -> Type -> ([TypeDef], Type)
prepare_unification` b db = prepare_unification b $ map getTypeDef $ allTypes db

levenshtein` :: String String -> Int
levenshtein` a b = if (indexOf a b == -1) 0 -100 +
	levenshtein [c \\ c <-: a] [c \\ c <-: b]

modLevenshtein :: String Module -> Int
modLevenshtein s mod
| s == mod        = -100
| isMember s path = -100 + length path
| otherwise       = length path + minList (map (levenshtein` s) path)
where path = split "." mod

modToFilename :: String -> String
modToFilename mod = (toString $ reverse $ takeWhile ((<>)'.')
                              $ reverse $ fromString mod) + ".dcl"

isUnifiable :: !Type !CloogleDB !FunctionEntry -> Bool
isUnifiable t1 db {fe_type} = isJust $ unify [] t1 =<<
	snd <$> prepare_unification` False db <$> fe_type

isNameMatch :: !Int !String !String -> Bool
isNameMatch maxdist n1 name
	# (n1, n2) = ({toLower c \\ c <-: n1}, {toLower c \\ c <-: name})
	= n1 == "" || indexOf n1 n2 <> -1 || levenshtein [c \\ c <-: n1] [c \\ c <-: n2] <= maxdist

isModNameMatch :: !Int !String !Module -> Bool
isModNameMatch maxdist name mod
	= isNameMatch maxdist name mod || isMember name (split "." mod)

isModMatch :: ![String] Location -> Bool
isModMatch mods (Location _ mod _ _ _) = isMember mod mods
isModMatch _    (Builtin _)            = False

isLibMatch :: ![String] Location -> Bool
isLibMatch libs (Location lib _ _ _ _) = any (\l -> indexOf l lib == 0) libs
isLibMatch _    (Builtin _)            = True

loc :: Location -> LocationResult
loc (Location lib mod ln iln _) = (lib, mod, ln, iln)
