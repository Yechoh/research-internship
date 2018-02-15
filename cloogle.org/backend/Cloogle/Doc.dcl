definition module Doc

/**
 * Parsing and storing Clean documentation
 */

from Data.Either import :: Either
from Data.Maybe import :: Maybe

/**
 * Documentation on a Clean syntax element
 */
:: Documentation
	= ModuleDoc (Maybe Description)
		//* Documentation for a module
	| FunctionDoc (Maybe Description) [ParamDoc] [VarDoc] (Maybe ResultDoc)
		//* Documentation for a function, generic or macro
	| ClassDoc (Maybe Description) [VarDoc] [Maybe ClassMemberDoc]
		//* Documentation for a class
	| TypeDoc (Maybe Description) [VarDoc] (Maybe TypeRhsDoc)
		//* Documentation for a type definition
	| ConstructorDoc (Maybe Description) [ParamDoc]
		//* Documentation for a type constructor

/**
 * Documentation for a Clean class member
 */
:: ClassMemberDoc
	= ClassMemberDoc (Maybe Description) [ParamDoc] (Maybe ResultDoc)

/**
 * Documentation for the right-hand side of a Clean type definition
 */
:: TypeRhsDoc
	= ADTDoc [Maybe Documentation]
		//* For an algebraic data type. Only uses ConstructorDoc.
	| RecordDoc [Maybe RecordFieldDoc]
		//* For a record
	| SynonymDoc (Maybe Description)
		//* For a type synonym

/**
 * Documentation on the parameter of a function, generic, macro or class member
 */
:: ParamDoc :== Description

/**
 * Documentation on a type variable for a type definition, class or generic
 */
:: VarDoc :== Description

/**
 * Documentation on a function result
 */
:: ResultDoc :== Description

/**
 * Documentation on a record field
 */
:: RecordFieldDoc :== Description

/**
 * Description of a Clean syntax element
 */
:: Description :== String

/**
 * Parse error for parsing Clean documentation; no documentation could be found
 */
:: ParseError
	= MissingAsterisk String //* At least one line did not start with a *
	| IllegalState String    //* Incorrect usage of this module

/**
 * Parse warning while parsing Clean documentation; the parser has made a
 * best-effort result nevertheless
 */
:: ParseWarning
	= UnknownField String //* Unknown @-field
	| IllegalField String //* This @-field is not allowed in this docblock
	| NoDescription       //* The main description is missing
	| UsedReturn          //* Used @return instead of @result

/**
 * Get the main description from a documentation
 */
getDocDescription :: Documentation -> Maybe Description

/**
 * Get the documentation of the parameters of a documentation block
 *
 * @result All ParamDocs on the top level. If none can be given for the
 *   constructor of the parameter, the empty list is returned.
 */
getParamDocs :: Documentation -> [ParamDoc]

/**
 * Get the documentation of the type variables of a documentation block
 *
 * @result All VarDocs on the top level. If none can be given for the
 *   constructor of the parameter, the empty list is returned.
 */
getVarDocs :: Documentation -> [VarDoc]

/**
 * Get the documentation on the result of a FunctionDoc
 *
 * @result The ResultDoc or Nothing if the parameter was not of the right
 *   constructor or there was no ResultDoc in the FunctionDoc.
 */
getResultDoc :: Documentation -> Maybe String

/**
 * Get the documentation on the right-hand side of a TypeDoc
 *
 * @result The documentation on the right-hand side, or Nothing if the
 *   parameter was not a TypeDoc or had no documentation on the RHS.
 */
getTypeRhsDoc :: Documentation -> Maybe TypeRhsDoc

/**
 * Get the record fields of the TypeDoc of a record type
 *
 * @result The record fields or Nothing if the parameter was not of the right
 *   constructor.
 */
getFieldDoc :: Documentation -> Maybe [Maybe RecordFieldDoc]

/**
 * Get the constructors of the TypeDoc of an algebraic data type
 *
 * @result The constructors or Nothing if the parameter was not of the right
 *   constructor.
 */
getConstructorDoc :: Documentation -> Maybe [Maybe Documentation]

/**
 * Get the representation of the TypeDoc of a synonym type
 *
 * @result The description of the synonym type or Nothing if the parameter was
 *   not of the right constructor or there was no representation information.
 */
getRepresentationDoc :: Documentation -> Maybe Description

/**
 * Convert a FunctionDoc to a ClassMemberDoc.
 * This gives a run-time error if the first parameter is not a FunctionDoc.
 */
functionToClassMemberDoc :: Documentation -> ClassMemberDoc

/**
 * Add a class member to an existing class definition
 *
 * @param The documentation to add the member to. Should be a ClassDoc, else a
 *   run-time error will occur
 * @param The documentation on the class member
 * @result The new ClassDoc
 */
addClassMemberDoc :: Documentation (Maybe ClassMemberDoc) -> Documentation

/**
 * Parse a string as documentation about a module
 */
parseModuleDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))

/**
 * Parse a string as documentation about a function definition
 */
parseFunctionDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))

/**
 * Parse a string as documentation about a class definition
 */
parseClassDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))

/**
 * Parse a string as documentation about an abstract type definition
 */
parseAbstractTypeDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))

/**
 * Parse a string as documentation about a synonym type definition
 */
parseSynonymTypeDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))

/**
 * Parse a string as documentation about an algebraic data type definition
 */
parseADTypeDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))

/**
 * Parse a string as documentation about a record type definition
 */
parseRecordTypeDoc :: (String -> Either ParseError (Documentation, [ParseWarning]))

/**
 * Parse documentation about a constructor
 *
 * @param The documentation to parse
 * @param A TypeDoc with an ADTDoc to add the fields to
 * @result The updated documentation
 */
parseConstructorDoc :: String Documentation
	-> Either ParseError (Documentation, [ParseWarning])

/**
 * Parse documentation about a record field
 *
 * @param The documentation to parse
 * @param A TypeDoc with a RecordDoc to add the fields to
 * @result The updated documentation
 */
parseFieldDoc :: String Documentation
	-> Either ParseError (Documentation, [ParseWarning])

/**
 * Trace a list of ParseWarnings like StdDebug might do it
 */
traceParseWarnings :: [ParseWarning] !a -> a

/**
 * Trace a ParseError like StdDebug might do it
 */
traceParseError :: ParseError !a -> a
