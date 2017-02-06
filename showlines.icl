module showlines


/**
 * CleanIDE imports
 */
import /*IdeState, logfile, PmDriver, PmEnvironment, PmProject, set_return_code,*/ UtilIO/*, UtilStrictLists*/

/**
 * Clean Platform imports
 */
import Data.Func, Data.Error, Data.List, Data.Void
import System.Directory, System.File, System.FilePath

/**
 * Clean libraries imports
 */
import StdBool, StdEnum, StdMisc, StdTuple, StdArray

/**
 * Given a list of strings, concatenate them to a single string with newlines
 * in between, then print that new string to console.
 */
showLines :: ![String] !*World -> *World
showLines lines world
  # (console, world) = stdio world
  # console          = seqSt (\s -> fwrites (s +++ "\n")) lines console
  = snd $ fclose console world

Start world = showLines ["hey,","jij","daar","!"] world
