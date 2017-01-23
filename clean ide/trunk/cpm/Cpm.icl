/**
 * CPM: Clean Project Manager
 *
 * CPM is a tool for managing CleanIDE-compatible projects on the commandline
 * and is targeted at OS X and Linux users who do not have access to the
 * CleanIDE.
 */
module Cpm

/**
 * CPM imports
 */
import AbsSyn, CpmLogic, Parser

/**
 * CleanIDE imports
 */
import UtilIO

/**
 * Clean Platform imports
 */
import System.CommandLine, System.Environment, System.Directory, System.FilePath
import Data.Error, Data.Func, Data.List
import Text

/**
 * Clean libraries imports
 */
import StdFile, StdString, StdMisc

/**
 * Start function which reads the program arguments, starts the parser and
 * starts processing the parse results.
 */
Start :: *World -> *World
Start world
  # (cmd, world)   = getCommandLine world
    (mpwd, world)  = getCurrentDirectory world
    (cpmd, world)  = accFiles GetFullApplicationPath world
    cleandir       = if (endsWith "bin" cpmd) (takeDirectory cpmd) cpmd
    (ch, world)    = case getEnvironmentVariable "CLEAN_HOME" world of
                       (Just ch, world)  -> (ch, world)
                       (_, world)        -> (cleandir, world)
  = case mpwd of
      Ok pwd   -> doCpmAction ch pwd (startParse (fromString $ mkCl cmd)) world
      Error e  -> abort "Failed to read current directory"
  where mkCl cmd = concat (intersperse " " (tl [fromString arg \\ arg <- cmd]))
