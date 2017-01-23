definition module CpmLogic

/**
 * CPM imports
 */
import AbsSyn

/**
 * CleanIDE imports
 */
from PmProject import :: Project

/**
 * Execute a general CPM action
 */
doCpmAction :: String String !CpmAction !*World -> *World

/**
 * Find all project files in the current working directory and build them
 */
doMake :: String !String !*World -> *World

/**
 * Execute project-specific actions
 */
doProjectAction :: String String String ProjectAction *World -> *World

/**
 * Execute path-related project actions
 */
doProjectPathAction :: String String Project PathAction *World -> *World

/**
 * Execute module-related actions
 */
doModuleAction :: String !String !ModuleAction !*World -> *World

/**
 * Turn a project name into a project filename
 */
mkProjectFile :: !String -> String

/**
 * Modify a project
 */
withProject :: !String !String (Project -> Project) *World -> *World

/**
 * Collect all project paths in a list with an index prefixed
 */
showPaths :: !Project -> [String]

/**
 * Modify the list of paths in a project given a modification function which
 * takes a strict list of project paths and returns a strict list of project
 * paths.
 */
doModPaths :: !String !String !Project ([!String!] -> [!String!]) *World -> *World

/**
 * Open a project file
 */
openProject :: !FilePath !FilePath !*World -> (!Project, !*World)

/**
 * Save a project back to its project file
 */
saveProject :: !FilePath !Project !FilePath !*World -> *World

/**
 * Remove an item from a strict list at a given index. Abort execution if the
 * index is out of bounds.
 */
rmStrictListIdx :: !Int [!a!] -> [!a!]

/**
 * Move a path at a given index up or down the list of paths. Abort execution
 * if the index is out of bounds.
 */
moveStrictListIdx :: !Int PathDirection [!a!] -> [!a!]

/**
 * Show an error message
 */
error :: !String !*World -> *World

/**
 * Show a help message
 */
help :: !String ![String] !*World -> *World

/**
 * Given a list of strings, concatenate them to a single string with newlines
 * in between, then print that new string to console.
 */
showLines :: ![String] !*World -> *World
