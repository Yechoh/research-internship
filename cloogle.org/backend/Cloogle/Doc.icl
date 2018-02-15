implementation module Doc

import StdChar
import StdDebug
from StdFunc import flip, o, twice
import StdList
import StdMisc
import StdString

import Control.Monad
import Data.Either
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple

:: DocField
	= Param String
	| Var String
	| Result String
	| Representation String

:: FieldUpdateFunction :==
	DocField Documentation -> Either ParseError (Documentation, [ParseWarning])

fieldName :: DocField -> String
fieldName (Param _)          = "param"
fieldName (Var _)            = "var"
fieldName (Result _)         = "result"
fieldName (Representation _) = "representation"

getDocDescription :: Documentation -> Maybe Description
getDocDescription (ModuleDoc d)         = d
getDocDescription (FunctionDoc d _ _ _) = d
getDocDescription (ClassDoc d _ _)      = d
getDocDescription (TypeDoc d _ _)       = d
getDocDescription (ConstructorDoc d _)  = d

getParamDocs :: Documentation -> [ParamDoc]
getParamDocs (FunctionDoc _ ps _ _) = ps
getParamDocs (ConstructorDoc _ ps)  = ps
getParamDocs _                      = []

getVarDocs :: Documentation -> [VarDoc]
getVarDocs (FunctionDoc _ _ vs _) = vs
getVarDocs (ClassDoc _ vs _)      = vs
getVarDocs (TypeDoc _ vs _)       = vs
getVarDocs _                      = []

getResultDoc :: Documentation -> Maybe String
getResultDoc (FunctionDoc _ _ _ r) = r
getResultDoc _                     = Nothing

getTypeRhsDoc :: Documentation -> Maybe TypeRhsDoc
getTypeRhsDoc (TypeDoc _ _ rhs) = rhs
getTypeRhsDoc _                 = Nothing

getFieldDoc :: Documentation -> Maybe [Maybe RecordFieldDoc]
getFieldDoc doc = case getTypeRhsDoc doc of
	Just (RecordDoc fs) = Just fs
	_                   = Nothing

getConstructorDoc :: Documentation -> Maybe [Maybe Documentation]
getConstructorDoc doc = case getTypeRhsDoc doc of
	Just (ADTDoc cs) = Just cs
	_                = Nothing

getRepresentationDoc :: Documentation -> Maybe Description
getRepresentationDoc doc = case getTypeRhsDoc doc of
	Just (SynonymDoc d) = d
	_                   = Nothing

functionToClassMemberDoc :: Documentation -> ClassMemberDoc
functionToClassMemberDoc (FunctionDoc d ps _ r) = ClassMemberDoc d ps r

addClassMemberDoc :: Documentation (Maybe ClassMemberDoc) -> Documentation
addClassMemberDoc (ClassDoc d vs ms) m = ClassDoc d vs (ms ++ [m])

parseModuleDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))
parseModuleDoc = parseDocBlock $ ModuleDoc Nothing

parseFunctionDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))
parseFunctionDoc = parseDocBlock $ FunctionDoc Nothing [] [] Nothing

parseClassDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))
parseClassDoc = parseDocBlock $ ClassDoc Nothing [] []

parseAbstractTypeDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))
parseAbstractTypeDoc = parseDocBlock $ TypeDoc Nothing [] Nothing

parseSynonymTypeDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))
parseSynonymTypeDoc = parseDocBlock $ TypeDoc Nothing [] $ Just $ SynonymDoc Nothing

parseADTypeDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))
parseADTypeDoc = parseDocBlock $ TypeDoc Nothing [] $ Just $ ADTDoc []

parseRecordTypeDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))
parseRecordTypeDoc = parseDocBlock $ TypeDoc Nothing [] $ Just $ RecordDoc []

parseConstructorDoc :: String Documentation
	-> Either ParseError (Documentation, [ParseWarning])
parseConstructorDoc s (TypeDoc d vs (Just (ADTDoc cs)))
	= appFst (TypeDoc d vs o Just o ADTDoc o (\c -> [c:cs]) o Just)
		<$> parseDocBlock (ConstructorDoc Nothing []) s
parseConstructorDoc _ _
	= Left $ IllegalState "parseConstructorDoc"

parseFieldDoc :: String Documentation
	-> Either ParseError (Documentation, [ParseWarning])
parseFieldDoc s (TypeDoc d vs (Just (RecordDoc fs)))
	= prepareString s >>= \ls ->
		Right (TypeDoc d vs $ Just $ RecordDoc [Just $ linesToString ls:fs], [])
parseFieldDoc _ _
	= Left $ IllegalState "parseFieldDoc"

updateDesc :: [[Char]] Documentation -> Documentation
updateDesc desc doc = case doc of
	FunctionDoc _ ps vs r = FunctionDoc desc` ps vs r
	ModuleDoc _           = ModuleDoc desc`
	ClassDoc _ vs cms     = ClassDoc desc` vs cms
	TypeDoc _ vs rhs      = TypeDoc desc` vs rhs
	ConstructorDoc _ ps   = ConstructorDoc desc` ps
where
	desc` = Just $ linesToString desc

parseDocBlock :: Documentation String
	-> Either ParseError (Documentation, [ParseWarning])
parseDocBlock doc block = prepareString block >>= parsef
where
	parsef :: [[Char]] -> Either ParseError (Documentation, [ParseWarning])
	parsef [] = Right (doc, [])
	parsef lines = case span ((<>) '@' o hd) lines of
		([],   rest) = appSnd (\ws -> [NoDescription:ws]) <$> parseFields doc rest
		(desc, rest) = appFst (updateDesc desc) <$> parseFields doc rest

	parseFields :: Documentation [[Char]]
		-> Either ParseError (Documentation, [ParseWarning])
	parseFields org []
		= Right (org, [])
	parseFields org [['@':line]:rest]
		= parseFields org rest` >>=
			\(d,ws) -> appSnd ((++) ws) <$> parseFs field desc d
	where
		(field, descline) = span (not o isSpace) line
		(restdesc, rest`) = span ((<>) '@' o hd) rest
		desc = flatten $ intersperse ['\n'] [descline:restdesc]

		parseFs :: [Char] [Char] Documentation
			-> Either ParseError (Documentation, [ParseWarning])
		parseFs ['param'] desc doc
			= upd (Param $ prepField desc) doc
		parseFs ['result'] desc doc
			= upd (Result $ prepField desc) doc
		parseFs ['return'] desc doc
			= appSnd ((++) [UsedReturn]) <$> parseFs ['result'] desc doc
		parseFs ['var'] desc doc
			= upd (Var $ prepField desc) doc
		parseFs ['representation'] desc doc
			= upd (Representation $ prepField desc) doc
		parseFs field _ doc
			= Right (doc, [UnknownField $ toString ['@':field]])

	upd :: DocField Documentation
		-> Either ParseError (Documentation, [ParseWarning])
	upd (Param d) (FunctionDoc t ps vs r) = Right (FunctionDoc t [d:ps] vs r, [])
	upd (Result d) (FunctionDoc t ps vs _) = Right (FunctionDoc t ps vs (Just d), [])
	upd (Var d) (FunctionDoc t ps vs r) = Right (FunctionDoc t ps [d:vs] r, [])
	upd (Param d) (ConstructorDoc t ps) = Right (ConstructorDoc t [d:ps], [])
	upd (Var d) (ClassDoc t vs ms) = Right (ClassDoc t [d:vs] ms, [])
	upd (Var d) (TypeDoc t vs rhs) = Right (TypeDoc t [d:vs] rhs, [])
	upd (Representation d) (TypeDoc t vs (Just (SynonymDoc _)))
		= Right (TypeDoc t vs $ Just $ SynonymDoc $ Just d, [])
	upd f doc = Right (doc, [IllegalField $ fieldName f])

	prepField :: ([Char] -> String)
	prepField = toString o trim

	concatWarnings :: FieldUpdateFunction DocField (Documentation, [ParseWarning])
		-> Either ParseError (Documentation, [ParseWarning])
	concatWarnings f df (doc, ws) = appSnd ((++) ws) <$> f df doc

prepareString :: String -> Either ParseError [[Char]]
prepareString s
	= filter (not o isEmpty) <$>
		( checkAsterisks
		$ filter (not o isEmpty)
		$ map (twice (reverse o trim))
		$ break '\n'
		$ fromString s
		)
where
	checkAsterisks :: [[Char]] -> Either ParseError [[Char]]
	checkAsterisks lines
	| all ((==) '*' o hd) lines
		= Right $ map (trim o tl) lines
		= Left $ MissingAsterisk $ toString $ hd $ filter ((<>) '*' o hd) lines

	break :: a -> [a] -> [[a]] | == a
	break e = foldr f []
	where
		f x []     = if (x == e) []        [[x]]
		f x [y:ys] = if (x == e) [[]:y:ys] [[x:y]:ys]

linesToString :: ([[Char]] -> String)
linesToString = toString o flatten o intersperse ['\n']

trim :== dropWhile isSpace

instance toString ParseWarning
where
	toString (UnknownField f) = "Doc warning: unknown field '" +++ f +++ "'"
	toString (IllegalField f) = "Doc warning: illegal field '" +++ f +++ "'"
	toString NoDescription    = "Doc warning: missing description"
	toString UsedReturn       = "Doc warning: @return is deprecated, use @result"

instance toString ParseError
where
	toString (MissingAsterisk l) = "Doc error: missing leading asterisk in '" +++ l +++ "'"
	toString (IllegalState s)    = "Doc error: illegal state: " +++ s

traceParseWarnings :: [ParseWarning] !a -> a
traceParseWarnings []     x = x
traceParseWarnings [w:ws] x
| trace_tn w = traceParseWarnings ws x
| otherwise  = undef

traceParseError :: ParseError !a -> a
traceParseError e x
| trace_tn e = x
| otherwise  = undef
