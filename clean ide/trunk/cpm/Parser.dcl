definition module Parser

/**
 * CPM imports
 */
import AbsSyn

/**
 * Clean Platform imports
 */
import Text.Parsers.ZParsers.ParsersKernel, Text.Parsers.ZParsers.ParsersDerived


/**
 * Parse the a list of characters to get the action to be executed. If parsing
 * fails, CpmHelp is returned as default action so help may be displayed.
 */
startParse :: [.Char] -> CpmAction

/**
 * Parse one or more non-whitespace characters
 */
pNotSpace :: Parser Char a [Char]

/**
 * Wrapper around the token parser that converts a Clean string to a list of
 * charactersm for easier parsing
 */
spstrtok :: (String -> Parser Char a [Char])

/**
 * Top-level parser for CPM commands
 */
pCpm :: Parser Char a CpmAction

/**
 * Parser for the project commands
 */
pProject :: Parser Char a CpmAction

/**
 * Parser for all path-related actions
 */
pPathAction :: Parser Char a ProjectAction

/**
 * Parser for constant mappings between text and constructors
 */
pConstCtr :: [(String, c)] -> Parser Char a c

/**
 * Parser to toggle the --force flag
 */
pForce :: Parser Char a Bool

/**
 * Parser for the argument to specify where the IDEEnvs file is
 */
pIDEEnvs :: Parser Char a String

/**
 * Parser for module-related actions
 */
pModule :: Parser Char a CpmAction

/**
 * Parser for the help command
 */
pHelp :: c -> Parser Char a c
