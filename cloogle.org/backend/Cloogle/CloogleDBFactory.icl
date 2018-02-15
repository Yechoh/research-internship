implementation module CloogleDBFactory

import StdArray
import StdBool
import StdFile
from StdFunc import const, flip, id, o
import StdList
import StdMisc
import StdOverloadedList
import StdString
import StdTuple

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Error
from Data.Func import $, mapSt
import Data.Functor
import Data.Maybe
import Data.Tuple
import System.Directory
from Text import class Text(concat,indexOf,replaceSubString), instance Text String

import CleanPrettyPrint

from compile import :: DclCache{hash_table}, empty_cache
from hashtable import :: BoxedIdent{boxed_ident}, :: HashTable{hte_symbol_heap},
	:: IdentClass(IC_Module), :: QualifiedIdents(NoQualifiedIdents),
	putIdentInHashTable, set_hte_mark, newHashTable
from Heap import :: Heap, newHeap, sreadPtr
from parse import wantModule
from predef import init_identifiers
from syntax import :: ClassDef{class_args,class_context,class_ident,class_pos},
	:: FileName, :: FunctName, :: FunKind(FK_Macro), :: FunSpecials, :: GCF,
	:: GenericCaseDef{gc_gcf,gc_pos,gc_type}, :: GenericCaseFunctions(GCF),
	:: GenericDef{gen_ident,gen_pos,gen_type,gen_vars},
	:: Ident{id_name,id_info}, :: LineNr, :: Module{mod_defs,mod_ident},
	:: Optional(Yes,No), :: SymbolPtr, :: Ptr, :: SymbolTableEntry{ste_doc},
	:: ParsedDefinition(PD_Class,PD_Derive,PD_Function,PD_Generic,PD_Instance,
		PD_Instances,PD_Type,PD_TypeSpec,PD_Documentation),
	:: ParsedExpr, :: ParsedInstance{pi_ident,pi_pos,pi_types},
	:: ParsedInstanceAndMembers{pim_pi}, :: ParsedModule, :: ParsedTypeDef,
	:: Position(FunPos,LinePos,NoPos), :: Priority, :: Rhs, :: ATypeVar,
	:: RhsDefsOfType(ConsList,ExtensibleConses,SelectorList,TypeSpec,EmptyRhs),
	:: SymbolTable, :: SymbolTableEntry, :: SymbolType, :: Type, :: BITVECT,
	:: TypeContext, :: TypeDef{td_ident,td_pos,td_rhs}, :: TypeVar,
	:: ParsedConstructor{pc_doc}, :: ParsedSelector{ps_doc}, :: DocType,
	:: OptionalDoc

import CoclUtils
import qualified Type as T
from Type import instance == Type,
	class print(print), instance print Type, instance print Priority
from Cloogle import :: FunctionKind(..)
import qualified CloogleDB as DB
from CloogleDB import :: Location(Location), filterLocations,
	:: ModuleEntry{me_is_core,me_documentation},
	:: FunctionEntry{fe_type,fe_kind,fe_generic_vars,fe_priority,fe_representation,fe_documentation},
	instance zero FunctionEntry, instance zero ModuleEntry
from Doc import :: Documentation(FunctionDoc), :: ResultDoc, :: VarDoc,
	:: ParamDoc, :: Description, :: TypeRhsDoc(RecordDoc), :: RecordFieldDoc,
	:: ClassMemberDoc,
	:: ParseWarning(UsedReturn), :: ParseError(IllegalState),
	parseFunctionDoc, parseConstructorDoc, parseADTypeDoc, parseRecordTypeDoc,
	parseSynonymTypeDoc, parseAbstractTypeDoc, parseFieldDoc, parseClassDoc,
	parseModuleDoc, traceParseError, traceParseWarnings, getTypeRhsDoc,
	functionToClassMemberDoc, addClassMemberDoc

:: TemporaryDB
	= { temp_functions   :: ![[(!'DB'.Location, !'DB'.FunctionEntry)]]
	  , temp_classes     :: ![[(!'DB'.Location, !'DB'.ClassEntry)]]
	  , temp_instances   :: ![[(!'DB'.Name, ![(!'DB'.Type, !String)], !'DB'.Location)]]
	  , temp_types       :: ![[(!'DB'.Location, !'DB'.TypeDefEntry)]]
	  , temp_derivations :: ![[(!'DB'.Name, ![(!'DB'.Type, !String, !'DB'.Location)])]]
	  , temp_modules     :: ![(!'DB'.Library, !'DB'.Module, !ModuleEntry)]
	  }

newTemporaryDb :: TemporaryDB
newTemporaryDb
	= { temp_functions   = []
	  , temp_classes     = []
	  , temp_instances   = []
	  , temp_types       = []
	  , temp_derivations = []
	  , temp_modules     = []
	  }

finaliseDb :: !TemporaryDB !'DB'.CloogleDB -> 'DB'.CloogleDB
finaliseDb tdb db
#! db = filterLocations (filterFun tdb.temp_modules) db
#! db = foldr (\(l,m,e) -> 'DB'.putModule l m e) db tdb.temp_modules
#! db = foldr 'DB'.putFunctions db tdb.temp_functions
#! db = foldr 'DB'.putClasses db tdb.temp_classes
#! db = foldr 'DB'.putTypes db tdb.temp_types
#! db = foldr 'DB'.putInstances db tdb.temp_instances
#! db = foldr 'DB'.putDerivationss db tdb.temp_derivations
= 'DB'.syncDb db
where
	filterFun :: ![(!'DB'.Library, !'DB'.Module, !ModuleEntry)] Location -> Bool
	filterFun mods ('DB'.Location l m _ _ _) = isEmpty [() \\ (l`,m`,_) <- mods | l == l` && m == m`]
	filterFun _    ('DB'.Builtin _)          = True

//             Exclude   Root    Library        Check for core       Base module
findModules :: ![String] !String !'DB'.Library ('DB'.Module -> Bool) !String !*World
	-> *(![('DB'.Library, 'DB'.Module, Bool)], !*World)
findModules ex root lib iscore base w
| any ((<>) -1 o flip indexOf path) ex = ([], w)
#! (fps, w)   = readDirectory path w
| isError fps = ([], w)
#! (Ok fps)   = fps
#! mods       = map (\s -> let mod = basedot +++ s % (0, size s - 5) in
	(lib, mod, iscore mod)) $ filter included $ filter isDclModule fps
#! (moremodss,w) = mapSt (findModules ex root lib iscore o ((+++) basedot)) (filter isDirectory fps) w
= (removeDup (mods ++ flatten moremodss), w)
where
	path = root +++ "/" +++ lib +++ if (base == "") "" "/" +++ replaceSubString "." "/" base
	basedot = if (base == "") "" (base +++ ".")

	included :: String -> Bool
	included s = not (any ((<>) -1 o flip indexOf (path +++ "/" +++ s)) ex)

	isDclModule :: String -> Bool
	isDclModule s = s % (size s - 4, size s - 1) == ".dcl"

	isDirectory :: (String -> Bool)
	isDirectory = not o isMember '.' o fromString

indexModule :: !String !'DB'.Module !'DB'.Library !Bool !TemporaryDB !*World
	-> *(!TemporaryDB, !*World)
indexModule root mod lib iscore db w
#! (lrdcl,symbols,w) = readModule False w
#! dcl = case lrdcl of
	Left a = abort a
	Right a = a
#! (icl,_,w) = readModule True w
#! icl = case icl of (Left _) = Nothing; (Right x) = Just x
#! modname = dcl.mod_ident.id_name
#! lib = lib % (0, size lib - size modname + size mod - 1)
#! typedefs = pd_types lib modname dcl.mod_defs icl symbols
#! db  =
	{ db
	& temp_functions =
		[ pd_typespecs lib modname dcl.mod_defs icl symbols
		, pd_macros lib modname dcl.mod_defs symbols
		, pd_generics lib modname dcl.mod_defs icl symbols
		, [f \\ td <- typedefs, f <- constructor_functions td]
		, [f \\ td <- typedefs, f <- record_functions td]
		: db.temp_functions
		]
	, temp_classes = [pd_classes lib modname dcl.mod_defs icl symbols:db.temp_classes]
	, temp_instances = [pd_instances lib modname dcl.mod_defs icl:db.temp_instances]
	, temp_types = [typedefs:db.temp_types]
	, temp_derivations = [pd_derivations lib modname dcl.mod_defs:db.temp_derivations]
	, temp_modules = [(lib,modname,pd_module iscore dcl.mod_defs):db.temp_modules]
	}
= (db,w)
where
	mkdir :: String -> String
	mkdir s = { if (c == '.') '/' c \\ c <-: s }

	pd_module :: Bool ![ParsedDefinition] -> ModuleEntry
	pd_module iscore [PD_Documentation _ doc:_]
		= { pd_module iscore []
		  & me_documentation = docParseResultToMaybe $ parseModuleDoc doc
		  }
	pd_module iscore _
		= { zero & me_is_core = iscore }

	pd_macros :: String String ![ParsedDefinition] SymbolTable
		-> [('DB'.Location, 'DB'.FunctionEntry)]
	pd_macros lib mod dcl st
		= [( 'DB'.Location lib mod (toLine pos) Nothing id.id_name
		   , { zero
		     & fe_kind=Macro
		     , fe_representation=Just $ priostring id +++ cpp pd
		     , fe_priority=findPrio id >>= 'T'.toMaybePriority
		     , fe_documentation=findDoc parseFunctionDoc id st
		     }
		   ) \\ pd=:(PD_Function pos id isinfix args rhs FK_Macro) <- dcl]
	where
		priostring :: Ident -> String
		priostring id = case findTypeSpec id dcl of
			Nothing    = ""
			(Just pri) = cpp pri +++ "\n"

		findPrio :: Ident -> Maybe Priority
		findPrio id = (\(PD_TypeSpec _ _ p _ _) -> p) <$> findTypeSpec id dcl

		findTypeSpec :: Ident [ParsedDefinition] -> Maybe ParsedDefinition
		findTypeSpec _  []          = Nothing
		findTypeSpec id [pd=:(PD_TypeSpec _ id` prio _ _):dcl]
		| id`.id_name == id.id_name = Just pd
		findTypeSpec id [_:dcl]     = findTypeSpec id dcl

	pd_derivations :: String String ![ParsedDefinition]
		-> [('DB'.Name, [('DB'.Type, String, 'DB'.Location)])]
	pd_derivations lib mod dcl
		= [( id.id_name
		   , [('T'.toType gc_type, cpp gc_type, 'DB'.Location lib mod (toLine gc_pos) Nothing "")]
		   ) \\ PD_Derive gcdefs <- dcl, {gc_type,gc_pos,gc_gcf=GCF id _} <- gcdefs]

	pd_generics :: String String ![ParsedDefinition] !(Maybe ParsedModule) SymbolTable
		-> [('DB'.Location, 'DB'.FunctionEntry)]
	pd_generics lib mod dcl icl st
		= [( 'DB'.Location lib mod (toLine gen_pos) (findIclLine id_name =<< icl) id_name
		   , { zero
		     & fe_type=Just $ 'T'.toType gen_type
		     , fe_generic_vars=Just $ map 'T'.toTypeVar gen_vars
		     , fe_representation=Just $ cpp gen
		     , fe_documentation=findDoc parseFunctionDoc id st
		     }
		   ) \\ gen=:(PD_Generic {gen_ident=id=:{id_name},gen_pos,gen_type,gen_vars}) <- dcl]
	where
		findIclLine :: String ParsedModule -> Maybe Int
		findIclLine name {mod_defs=pms}
			= case [g.gen_pos \\ PD_Generic g <- pms | g.gen_ident.id_name == name] of
				[FunPos _ l _:_] = Just l
				[LinePos _ l:_] = Just l
				_ = Nothing

	pd_typespecs :: String String ![ParsedDefinition] !(Maybe ParsedModule) SymbolTable
		-> [('DB'.Location, 'DB'.FunctionEntry)]
	pd_typespecs lib mod dcl icl st
		= [( 'DB'.Location lib mod (toLine pos) (findIclLine id_name =<< icl) id_name
		   , { zero
		     & fe_type=Just $ 'T'.toType t
		     , fe_priority = 'T'.toMaybePriority p
		     , fe_representation = Just $ cpp ts
		     , fe_documentation = findDoc parseFunctionDoc id st
		     }
		   ) \\ ts=:(PD_TypeSpec pos id=:{id_name} p (Yes t) funspecs) <- dcl]
	where
		findIclLine :: String ParsedModule -> Maybe Int
		findIclLine name {mod_defs=pms}
			= case [pos \\ PD_TypeSpec pos id _ _ _ <- pms | id.id_name == name] of
				[FunPos _ l _:_] = Just l
				[LinePos _ l:_] = Just l
				_ = Nothing

	pd_instances :: String String ![ParsedDefinition] !(Maybe ParsedModule)
		-> [('DB'.Name, [('DB'.Type, String)], 'DB'.Location)]
	pd_instances lib mod dcl icl
		= [( id
		   , types
		   , 'DB'.Location lib mod (toLine pos) (findIclLine id (map fst types) =<< icl) ""
		   ) \\ (id,types,pos) <- instances]
	where
		instances = map (appSnd3 (map (\t -> ('T'.toType t, cppp t)))) $
			[(i.pi_ident.id_name, i.pi_types, i.pi_pos) \\ PD_Instance {pim_pi=i} <- dcl]
			++ [(i.pi_ident.id_name, i.pi_types, i.pi_pos) \\ PD_Instances pis <- dcl, {pim_pi=i} <- pis]

		findIclLine :: String ['T'.Type] ParsedModule -> Maybe Int
		findIclLine name types {mod_defs=pms}
			= case [pi_pos
					\\ PD_Instance {pim_pi={pi_pos,pi_ident,pi_types}} <- pms
					| (pi_ident.id_name == name && map 'T'.toType pi_types == types)] of
				[LinePos _ l:_] = Just l
				_ = Nothing

	pd_classes :: String String ![ParsedDefinition] !(Maybe ParsedModule) SymbolTable
		-> [('DB'.Location, 'DB'.ClassEntry)]
	pd_classes lib mod dcl icl st
	# dcl = filter (\pd->case pd of (PD_Class _ _)=True; _=False) dcl
	= map (\(PD_Class {class_ident=id=:{id_name},class_pos,class_args,class_context} dcl)
		-> let
			typespecs = pd_typespecs lib mod dcl icl st
			macros = pd_macros lib mod dcl st
			getMacro n = case filter ((==) n o 'DB'.getName o fst) macros of
				[]        = Nothing
				[(_,m):_] = m.fe_representation
			updateRepresentation n fe
				= {fe & fe_representation=getMacro n <|> fe.fe_representation}
		in ( 'DB'.Location lib mod (toLine class_pos) (findIclLine id_name =<< icl) id_name
		   , 'DB'.toClass
		       (map 'T'.toTypeVar class_args)
		       (flatten $ map 'T'.toClassContext class_context)
		       (flip (foldl addClassMemberDoc)
		           [functionToClassMemberDoc <$> fe.fe_documentation \\ (_,fe) <- typespecs]
		           <$> (findDoc parseClassDoc id st))
		       [(f,updateRepresentation f et) \\ ('DB'.Location _ _ _ _ f, et) <- typespecs]
		   )) dcl
	where
		findIclLine :: String ParsedModule -> Maybe Int
		findIclLine name {mod_defs=pms}
			= case [class_pos \\ PD_Class {class_ident,class_pos} _ <- pms | class_ident.id_name == name] of
				[LinePos _ l:_] = Just l
				_ = Nothing

	pd_types :: String String ![ParsedDefinition] !(Maybe ParsedModule) SymbolTable
		-> [('DB'.Location, 'DB'.TypeDefEntry)]
	pd_types lib mod dcl icl st
		= [let name = 'T'.td_name td in
			( 'DB'.Location lib mod (toLine ptd.td_pos) (findIclLine name =<< icl) name
			, 'DB'.toTypeDefEntry td $ findRhsDoc ptd =<<
				(case findDoc (parsef ptd.td_rhs) ptd.td_ident st of
					Just d  -> Just d
					Nothing -> docParseResultToMaybe $ parsef ptd.td_rhs ""
				)
			) \\ PD_Type ptd <- dcl, td <- ['T'.toTypeDef ptd]]
	where
		findIclLine :: String ParsedModule -> Maybe Int
		findIclLine name {mod_defs=pms}
			= case [td_pos \\ PD_Type {td_ident,td_pos} <- pms | td_ident.id_name == name] of
				[LinePos _ l:_] = Just l
				_ = Nothing

		parsef :: RhsDefsOfType -> String -> Either ParseError (Documentation, [ParseWarning])
		parsef (ConsList _)           = parseADTypeDoc
		parsef (ExtensibleConses _)   = parseADTypeDoc
		parsef (SelectorList _ _ _ _) = parseRecordTypeDoc
		parsef (TypeSpec _)           = parseSynonymTypeDoc
		parsef (EmptyRhs _)           = parseAbstractTypeDoc
		parsef _                      = const $ Left $ IllegalState "Unknown RhsDefsOfType"

		findRhsDoc :: ParsedTypeDef Documentation -> Maybe Documentation
		findRhsDoc {td_rhs=ConsList cs}           doc = addConses cs doc
		findRhsDoc {td_rhs=ExtensibleConses cs}   doc = addConses cs doc
		findRhsDoc {td_rhs=SelectorList _ _ _ fs} doc = addFields fs doc
		findRhsDoc _                              doc = Just doc

		addFields :: [ParsedSelector] Documentation -> Maybe Documentation
		addFields [] doc
			= Just doc
		addFields [{ps_doc=Yes d}:fs] doc
			= addFields fs doc >>= docParseResultToMaybe o parseFieldDoc d
		addFields [_:fs] doc
			= addFields fs doc

		addConses :: [ParsedConstructor] Documentation -> Maybe Documentation
		addConses [] doc
			= Just doc
		addConses [{pc_doc=Yes d}:cs] doc
			= addConses cs doc >>= docParseResultToMaybe o parseConstructorDoc d
		addConses [_:cs] doc
			= addConses cs doc

	toLine :: Position -> 'DB'.LineNr
	toLine (FunPos _ l _) = Just l
	toLine (LinePos _ l)  = Just l
	toLine _              = Nothing

	readModule :: Bool !*World -> *(!Either String ParsedModule, SymbolTable, !*World)
	readModule icl w
	# ht = newHashTable newHeap
	# ht = set_hte_mark (if icl 1 0) ht
	# filename = root +++ "/" +++ lib +++ "/" +++ mkdir mod +++ if icl ".icl" ".dcl"
	# (ok,f,w) = fopen filename FReadText w
	| not ok = (Left $ "Couldn't open " +++ filename, abort "no symboltable\n", w)
	# (mod_id, ht) = putIdentInHashTable mod (IC_Module NoQualifiedIdents) ht
	# ((b1,b2,pm,ht,f),w) = accFiles (wantModule` f "" icl mod_id.boxed_ident NoPos True ht stderr) w
	# (ok,w) = fclose f w
	| not ok = (Left $ "Couldn't open " +++ filename, abort "no symboltable\n", w)
	= (Right pm, ht.hte_symbol_heap, w)
	where
		wantModule` :: !*File !{#Char} !Bool !Ident !Position !Bool !*HashTable !*File !*Files
			-> ((!Bool,!Bool,!ParsedModule, !*HashTable, !*File), !*Files)
		wantModule` f s b1 i p b2 ht io fs
		# (b1,b2,pm,ht,f,fs) = wantModule f s b1 i p b2 ht io fs
		= ((b1,b2,pm,ht,f),fs)

	docParseResultToMaybe :: (Either ParseError (Documentation, [ParseWarning]))
		-> Maybe Documentation
	docParseResultToMaybe (Left e)
		= traceParseError e Nothing
	docParseResultToMaybe (Right (doc,ws))
		= traceParseWarnings (filter (not o isUsedReturn) ws) (Just doc)

	findDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))
		Ident SymbolTable -> Maybe Documentation
	findDoc parse {id_info} st = case sreadPtr id_info st of
		{ste_doc=Yes doc} = docParseResultToMaybe $ parse doc
		_                 = Nothing

	isUsedReturn :: ParseWarning -> Bool
	isUsedReturn UsedReturn = True; isUsedReturn _ = False

constructor_functions :: ('DB'.Location, 'DB'.TypeDefEntry)
	-> [('DB'.Location, 'DB'.FunctionEntry)]
constructor_functions (loc, etd)
	= [(loc` c,
		{ zero
		& fe_kind=Constructor
		, fe_type=Just f
		, fe_representation=Just $ concat $ [c] ++ print_prio p ++ [" :: "] ++ print False f
		, fe_priority=p
		})
		\\ (c,f,p) <- 'T'.constructorsToFunctions ('DB'.getTypeDef etd)]
where
	loc` c = case loc of
		'DB'.Builtin _ -> 'DB'.Builtin c
		'DB'.Location lib mod line iclline _ -> 'DB'.Location lib mod line iclline c

print_prio :: (Maybe 'T'.Priority) -> [String]
print_prio Nothing  = []
print_prio (Just p) = [" "] ++ print False p

record_functions :: ('DB'.Location, 'DB'.TypeDefEntry)
	-> [('DB'.Location, 'DB'.FunctionEntry)]
record_functions (loc, etd)
	= [(loc` f,
		{ zero
		& fe_kind=RecordField
		, fe_type=Just t
		, fe_representation=Just $ concat [".", f, " :: ":print False t]
		, fe_documentation=(\d -> FunctionDoc (Just d) [] [] Nothing) <$> doc
		})
		\\ (f,t) <- 'T'.recordsToFunctions ('DB'.getTypeDef etd)
		 & doc <- field_doc]
where
	loc` f = case loc of
		'DB'.Builtin _ -> 'DB'.Builtin f
		'DB'.Location lib mod line iclline _ -> 'DB'.Location lib mod line iclline f
	field_doc = case getTypeRhsDoc =<< 'DB'.getTypeDefDoc etd of
		Just (RecordDoc fields) = fields ++ repeat Nothing
		_                       = repeat Nothing
