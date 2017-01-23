implementation module Parser

/**
 * CPM imports
 */
import AbsSyn

/**
 * CleanIDE imports
 */
import PmEnvironment

/**
 * Clean Platform imports
 */
import Control.Applicative
import Data.List, Data.Maybe, Data.Functor
import Text.Parsers.ZParsers.ParsersKernel, Text.Parsers.ZParsers.ParsersDerived, Text.Parsers.ZParsers.ParsersAccessories

/**
 * Clean libraries imports
 */
import StdFunc, StdTuple


/**
 * Parse one or more non-whitespace characters
 */
pNotSpace :: Parser Char a [Char]
pNotSpace = ds (<+> (satisfy (not o space)))

/**
 * Top-level parser for CPM commands
 */
pCpm :: Parser Char a CpmAction
pCpm = //mkP ((\_ f -> Project "foo" (BuildProject f "foo")) <$> mkG (spstrtok "build") <||> mkG pForce)
       pMake <|> pProject <|> pModule <|> pQuickBuild <|> (yield CpmHelp)
  where pQuickBuild = pNotSpace <&> \pn -> pBuildOpts <@ Project (toString pn)

/**
 * Parse the make command
 */
pMake :: Parser Char a CpmAction
pMake = spstrtok "make" <@ const CpmMake

/**
 * Wrapper around the token parser that converts a Clean string to a list of
 * charactersm for easier parsing
 */
spstrtok :: (String -> Parser Char a [Char])
spstrtok = ds o tokenH o fromString

/**
 * Parser for the project commands
 */
pProject :: Parser Char a CpmAction
pProject = spstrtok "project" &> (pProjectWithName <!> yield (Project "" ProjectHelp))
  //where  pProjectWithName  =  mkP (Project o toString <$> mkG pNotSpace <||> mkG pProjectAction) //<&> \pn -> pProjectAction <@ Project (toString pn)
  where  pProjectWithName  =  pNotSpace <&> \pn -> pProjectAction <@ Project (toString pn)
         pProjectAction    =  (spstrtok "create"                                     <@ const CreateProject)
                         <|>  (spstrtok "show"                                       <@ const ShowProject)
                         //<|>  mkP ((\_ -> BuildProject) <$> mkG (spstrtok "build") <||> mkG pForce <||> mkG pIDEEnvs) // (spstrtok "build"   &> pForce      <&> \f -> pIDEEnvs  <@ BuildProject f)
                         <|>  (spstrtok "build"   &> pBuildOpts)
                         <|>  (spstrtok "path"    &> pPathAction)
                         <|>  (spstrtok "root"    &> pNotSpace                       <@ SetRelativeRoot o toString)
                         <|>  (spstrtok "target"  &> identifier                      <@ SetTarget o toString)
                         <|>  (spstrtok "exec"    &> identifier                      <@ SetExec o toString)
                         <!>  (pHelpYield ProjectHelp)

/**
 * Parse options for the build command
 */
pBuildOpts :: Parser Char a ProjectAction
pBuildOpts = pForce <&> \f -> pIDEEnvs <@ BuildProject f

/**
 * Parser for the environment commands
 */
pEnvironment :: Parser Char a CpmAction
pEnvironment = spstrtok "environment" &> pEnvironmentAction <@ Environment
  where  pEnvironmentAction  =  (spstrtok "list"                                               <@ const ListEnvironments)
                           <|>  (spstrtok "import"       &> pNotSpace                          <@ ImportEnvironment o toString)
                           <|>  (spstrtok "create"       &> identifier                         <@ CreateEnvironment o toString)
                           <|>  (spstrtok "remove"       &> identifier                         <@ RemoveEnvironment o toString)
                           <|>  (spstrtok "show"         &> identifier                         <@ ShowEnvironment o toString)
                           <|>  (spstrtok "export"       &> identifier                         <@ ExportEnvironment o toString)
                           <|>  (spstrtok "rename"       &> identifier  <&> \en -> identifier  <@ RenameEnvironment (toString en) o toString)
                           <|>  (spstrtok "setcompiler"  &> identifier  <&> \en -> identifier  <@ SetEnvironmentCompiler (toString en) o toString)
                           <|>  (spstrtok "setcodegen"   &> identifier  <&> \en -> identifier  <@ SetEnvironmentCodeGen (toString en) o toString)
                           <!>  (pHelpYield EnvironmentHelp)

/**
 * Parser for all path-related actions
 */
pPathAction :: Parser Char a ProjectAction
pPathAction = pPathAction <@ ProjectPath
   where  pPathAction  =  (spstrtok "add"     &>  pNotSpace  <@ AddPathAction o toString)
                     <|>  (spstrtok "remove"  &>  ds number  <@ RemovePathAction)
                     <|>  (spstrtok "list"                   <@ const ListPathsAction)
                     <|>  (spstrtok "move"    &>  pPathDirection)
                     <!>  (pHelpYield PathHelp)
          pPathDirection  = ds number <&> \i -> pConstCtr dirOpts <@ MovePathAction i
          dirOpts =  [  ("up", MovePathUp), ("down", MovePathDown)
                     ,  ("top", MovePathTop), ("bottom", MovePathBottom)]

/**
 * Parser for constant mappings between text and constructors
 */
pConstCtr :: [(String, c)] -> Parser Char a c
pConstCtr xs = choice (map (\(s, d) -> (spstrtok s <@ const d)) xs)

/**
 * Parser to toggle the --force flag
 */
pForce :: Parser Char a Bool
pForce = (spstrtok "--force" <@ const True) <!> (yield False)

/**
 * Parser for the argument to specify where the IDEEnvs file is
 */
pIDEEnvs :: Parser Char a String
pIDEEnvs =  spstrtok "--envs" &> (<?> (ds (symbol '=')) id '=') &> pNotSpace <@ toString
       <!>  (yield EnvsFileName)

/**
 * Parser for module-related actions
 */
pModule :: Parser Char a CpmAction
pModule = spstrtok "module" &> (pModuleWithName <!> yield (Module "" ModuleHelp))
  where  pModuleWithName  =  pNotSpace <&> \mn -> pModuleAction <@ Module (toString mn)
         pModuleAction    =  (spstrtok "create" &> pModuleType <@ CreateModule)
                        <!>  (pHelpYield ModuleHelp)
         pModuleType      =  (spstrtok "application" <@ const ApplicationModule)
                        <!>  (yield LibraryModule)

/**
 * Parser for the help command
 */
pHelp :: c -> Parser Char a c
pHelp c = spstrtok "help" <@ const c

pHelpYield :: c -> Parser Char a c
pHelpYield c = (spstrtok "help" <@ const c) <|> (yield c)

/**
 * Parse the a list of characters to get the action to be executed. If parsing
 * fails, CpmHelp is returned as default action so help may be displayed.
 */
startParse :: [.Char] -> CpmAction
startParse input =
  case parse pCpm input "line" "character" of
    Succ [x:_]  -> x
    _           -> CpmHelp
//startParse args = maybe CpmHelp snd (find (isnull o fst) (begin pCpm args))

