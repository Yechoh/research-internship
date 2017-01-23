implementation module CpmLogic

/**
 * CPM imports
 */
import AbsSyn, CpmPaths

/**
 * CleanIDE imports
 */
import IdeState, logfile, PmDriver, PmEnvironment, PmProject, set_return_code, UtilIO, UtilStrictLists

/**
 * Clean Platform imports
 */
import Text
import Data.Func, Data.Error, Data.List, Data.Void
import System.Directory, System.File, System.FilePath

/**
 * Clean libraries imports
 */
import StdBool, StdEnum, StdMisc, StdTuple, StdArray

/**
 * Execute a general CPM action
 */
doCpmAction :: String String !CpmAction !*World -> *World
doCpmAction cleanhome  pwd  CpmMake           world = doMake cleanhome pwd world
doCpmAction cleanhome  pwd  (Project pn pa)   world = doProjectAction cleanhome pwd pn pa world
doCpmAction cleanhome  pwd  (Module mn ma)    world = doModuleAction cleanhome mn ma world
doCpmAction cleanhome  pwd  (Environment ea)  world = doEnvironmentAction cleanhome pwd ea world
doCpmAction _          _    _                 world =
  help  "cpm <target>"
    [  "Where <target> is one of the following:"
    ,  "  <projectname> [--force] [--envs=filename]  : build project <projectname>."
    ,  "                                               Optionally force build (default: 'false')"
    ,  "                                               Optionally specify the environments file (default: 'IDEEnvs')"
    ,  "  project <projectname>                      : project actions"
    ,  "  module <modulename>                        : module actions"
    //,  "  environment                                : environment actions"
    ,  "  make                                       : build all projects in the current directory"
    ,  ""
    ,  "Execute `cpm <target> help` to get help for specific actions."] world

/**
 * Find all project files in the current working directory and build them
 */
doMake :: String !String !*World -> *World
doMake cleanhome pwd world
  # (mbErr, world) = readDirectory pwd world
  = case mbErr of
      Error _     -> error "Failed to read current directory" world
      Ok entries  -> case filter (\entry -> endsWith ".prj" entry) entries of
                       []  -> error ("No project file found in " +++ pwd) world
                       xs  -> foldr (\pn -> doProjectAction cleanhome pwd pn (BuildProject False EnvsFileName)) world xs

/**
 * Default compiler options. Currently it is a simple alias for
 * forwards-compatibility.
 */
compilerOptions :: CompilerOptions
compilerOptions = DefaultCompilerOptions

getLine :: *World -> *(String, *World)
getLine world
  # (console, world)  = stdio world
  # (line, console)   = freadline console
  # (_, world)        = fclose console world
  = (line, world)

import StdDebug
/**
 * Execute project-specific actions
 */
doProjectAction :: String String String ProjectAction *World -> *World
doProjectAction cleanhome pwd  pn  CreateProject world
  //Check if main module exists
  # (exists,world)  = fileExists mainmodule world
  | not exists //       = error ("Main module " +++ mainmodule +++ " does not exist.") world
    # world         = showLines ["Main module " +++ mainmodule +++ " does not exist. Create it? [y/n]"] world
    # (line, world) = getLine world
    | line.[0] == 'y' = mkMainAndProject world
    | otherwise       = error ("Failed to create project. Need " +++ mainmodule) world
  | otherwise       = mkProject world
  where
  basefilename = dropExtension pn
  mainmodule   = addExtension basefilename "icl"

  mkMainAndProject world
    # world = doModuleAction "" mainmodule (CreateModule ApplicationModule) world
    = mkProject world
  mkProject world
    # edit_options   = {eo={newlines=NewlineConventionUnix},pos_size=NoWindowPosAndSize}
    //Create project file using the Clean IDE libraries
    # prj            = PR_NewProject mainmodule edit_options compilerOptions DefCodeGenOptions
                         DefApplicationOptions [!!] DefaultLinkOptions
    # project        = PR_SetRoot mainmodule edit_options compilerOptions prj
    # projectfile    = mkProjectFile basefilename //addExtension basefilename "prj"
    # (prjok, world) = accFiles (SaveProjectFile projectfile project cleanhome) world
    | not prjok      = error ("Could not create project file " +++ projectfile) world
    = world

doProjectAction cleanhome pwd  pn  ShowProject world
  # projectfile       = mkProjectFile pn
  # (project, world)  = openProject cleanhome projectfile world
  = showLines  [  "Content of " +++ projectfile +++ ":"
               ,  "ProjectRoot..: " +++ PR_GetRelativeRootDir project
               ,  "Target.......: " +++ PR_GetTarget project
               ,  "Executable...: " +++ PR_GetExecPath project
               ,  "Paths........:"
               :  showPaths project
               ] world

doProjectAction cleanhome pwd  pn  (BuildProject force ideenvs) world
  # (envs, world)            = readIDEEnvs cleanhome ideenvs world
  # proj_path                = GetLongPathName (pwd </> pn)
  # ((proj, ok, err), world) = accFiles (ReadProjectFile proj_path cleanhome) world
  | not ok || err <> ""      = error ("CPM failed while opening project: " +++ err +++ "\n") world
  # (console, world)         = stdio world
  # iniGeneral               = initGeneral True compilerOptions cleanhome proj_path proj envs console
  # {ls, gst_world}          = pinit force {ls=iniGeneral,gst_world=world,gst_continue_or_stop=False}
  = gst_world
  where
  pinit force_rebuild gst = BringProjectUptoDate force_rebuild cleanup gst
  cleanup exepath bool1 bool2 ps = abortLog False "" ps

doProjectAction cleanhome  _  pn  (ProjectPath pa) world
  # projectfile       = mkProjectFile pn
  # (project, world)  = openProject cleanhome projectfile world
  = doProjectPathAction cleanhome projectfile project pa world

doProjectAction cleanhome pwd pn (SetRelativeRoot target) world =
  withProject cleanhome pn (PR_SetRelativeRootDir target) world

doProjectAction cleanhome pwd pn (SetTarget target) world =
  withProject cleanhome pn (PR_SetTarget target) world

doProjectAction cleanhome pwd pn (SetExec exec) world =
  withProject cleanhome pn (PR_SetExecPath exec) world

doProjectAction _          _  _   _    world             =
  help "cpm project <projectname> <action>"
    [  "Where <action> is one of the following"
    ,  "  create                              : create a new project"
    ,  "  show                                : show project information"
    ,  "  build [--force] [--envs=filename]   : build the project. Optionally force build (default: 'false')"
    ,  "                                        Optionally specify the environments file (default: 'IDEEnvs')"
    ,  "  path                                : manage project paths"
    ,  "  target <env>                        : set target environment to <env>"
    ,  "  exec <execname>                     : set executable name to <execname>"
    ] world

/**
 * Execute environment-specific actions
 */
doEnvironmentAction :: String String EnvironmentAction *World -> *World
doEnvironmentAction cleanhome  pwd  ListEnvironments                world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (ImportEnvironment ef)          world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (RemoveEnvironment en)          world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (ShowEnvironment en)            world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (ExportEnvironment en)          world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (CreateEnvironment en)          world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (RenameEnvironment en en`)      world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (SetEnvironmentCompiler en cp)  world = error ("Not implemented") world
doEnvironmentAction cleanhome  pwd  (SetEnvironmentCodeGen en cp)   world = error ("Not implemented") world
doEnvironmentAction _          _    _                               world =
  help "cpm environment <action>"
    [  "Where <action> is one of the following"
    ,  "  list                                  : list all available environments"
    ,  "  import <filepath>                     : import an environement from file <filepath>"
    ,  "  create <envname>                      : create a new environment with name <envname>"
    ,  "  remove <envname>                      : remove evironment <envname>"
    ,  "  show <envname>                        : show environment <envname>"
    ,  "  export <envname>                      : export environment <envname>"
    ,  "  rename <envname> <envname`>           : rename environment <envname> to <envname`>"
    ,  "  setcompiler <envname> <compilername>  : set compiler for <envname> to <compilername>"
    ,  "  setcodegen <envname> <codegenname>    : set codegen for <envname> to <codegenname>"
    ] world

/**
 * Turn a project name into a project filename
 */
mkProjectFile :: !String -> String
mkProjectFile pn = addExtension (dropExtension pn) "prj"

/**
 * Modify a project
 */
withProject :: !String !String (Project -> Project) *World -> *World
withProject cleanhome pn f world
  # projectfile      = mkProjectFile pn
  # (project, world) = openProject cleanhome projectfile world
  = saveProject cleanhome (f project) projectfile world

/**
 * Execute path-related project actions
 */
doProjectPathAction :: String String Project PathAction *World -> *World
doProjectPathAction cleanhome pn project (AddPathAction path) world =
  doModPaths cleanhome pn project ((:!) path) world

doProjectPathAction cleanhome pn project (RemovePathAction i) world =
  doModPaths cleanhome pn project (rmStrictListIdx i) world

doProjectPathAction _ _ project ListPathsAction world = showLines ["Paths for project:" : showPaths project] world

doProjectPathAction cleanhome pn project (MovePathAction i pdir) world =
  doModPaths cleanhome pn project (moveStrictListIdx i pdir) world

doProjectPathAction _         _   _     _  world =
  help "cpm project <projectname.prj> path <action>"
    [  "Where <action> is one of the following"
    ,  "  add <path>          : add a path to the project"
    ,  "  list                : list all project paths and their index"
    ,  "  remove <i>          : remove path <i> from the list of projects"
    ,  "  move <i> <up|down>  : move path <i> up or down one position" ] world

/**
 * Collect all project paths in a list with an index prefixed
 */
showPaths :: !Project -> [String]
showPaths project = map f (zip2 [0..] (StrictListToList (PR_GetPaths project)))
  where f (n, p)  = "  [" +++ toString n +++ "]  " +++ p

/**
 * Modify the list of paths in a project given a modification function which
 * takes a strict list of project paths and returns a strict list of project
 * paths.
 */
doModPaths :: !String !String !Project ([!String!] -> [!String!]) *World -> *World
doModPaths cleanhome pn project f world
  # paths = PR_GetPaths project
  # prj   = PR_SetPaths False paths (f paths) project
  # world = saveProject cleanhome prj pn world
  = showLines ["Successfully modified project paths"] world

/**
 * Open a project file
 */
openProject :: !FilePath !FilePath !*World -> (!Project, !*World)
openProject cleanhome projectfile world
  # ((prj, ok, err), world) = accFiles (ReadProjectFile projectfile cleanhome) world
  | ok        = (prj, world)
  | otherwise = (prj, error err world)

/**
 * Save a project back to its project file
 */
saveProject :: !FilePath !Project !FilePath !*World -> *World
saveProject cleanhome prj projectfile world
  # (ok, world) = accFiles (SaveProjectFile projectfile prj cleanhome) world
  | not ok    = error "Error saving project" world
  | otherwise = world

/**
 * Remove an item from a strict list at a given index. Abort execution if the
 * index is out of bounds.
 */
rmStrictListIdx :: !Int [!a!] -> [!a!]
rmStrictListIdx 0  (_ :! t)          = t
rmStrictListIdx n  (h :! t) | n > 0  = h :! (rmStrictListIdx (n - 1) t)
rmStrictListIdx n  _                 = abort ("Index " +++ toString n +++ " out of bounds")

/**
 * Move a path at a given index up or down the list of paths. Abort execution
 * if the index is out of bounds.
 */
moveStrictListIdx :: !Int PathDirection [!a!] -> [!a!]
moveStrictListIdx i dir xs
  | i < 0 || i > (LLength xs - 1)  = abort ("Index " +++ toString i +++ " out of bounds")
  | otherwise                      = ListToStrictList (msl dir (splitAt i (StrictListToList xs)))
  where  msl MovePathUp      ([], xs)        = xs
         msl MovePathUp      (xs, [x:ys])    = (init xs) ++ [x : (last xs) : ys]
         msl MovePathDown    ([], [x:y:ys])  = [y:x:ys]
         msl MovePathDown    (xs, [])        = xs
         msl MovePathDown    (xs, [y])       = xs ++ [y]
         msl MovePathDown    (xs, [x:y:ys])  = xs ++ [y:x:ys]
         msl MovePathTop     (xs, [])        = xs
         msl MovePathTop     (xs, [y:ys])    = [y:xs] ++ ys
         msl MovePathBottom  (xs, [])        = xs
         msl MovePathBottom  (xs, [y:ys])    = xs ++ ys ++ [y]

/**
 * Execute module-related actions
 */
doModuleAction :: String !String !ModuleAction !*World -> *World
doModuleAction _ mn  (CreateModule mt) world
  # (dclexists, world)  = fileExists dclnm world
  | dclexists           = error ("Definition module '" +++ dclnm +++ "' already exists.") world
  # (iclexists, world)  = fileExists iclnm world
  | iclexists           = error ("Implementation module '" +++ iclnm +++ "' already exists.") world
  = writeMods mt world
  where
  basenm     = dropExtension mn
  dclnm      = addExtension basenm "dcl"
  iclnm      = addExtension basenm "icl"

  mkmod mty  = mty +++ "module " +++ basenm

  writeMods ApplicationModule world = writeicl ApplicationModule world
  writeMods LibraryModule world
    # world = writeicl ApplicationModule world
    = writedcl world

  writeicl ApplicationModule  world = writeicl` "" world
  writeicl LibraryModule      world = writeicl` "implementation " world

  writeicl` pref world = writemod iclnm pref ("Failed to write implementation module '" +++ basenm +++ "'") world

  writedcl world = writemod dclnm "definition " ("Failed to write definition module '" +++ basenm +++ "'") world

  writemod nm pref errmsg world
    # (me, world)  = writeFile nm (mkmod pref) world
    | isError me   = error errmsg world
    = world

doModuleAction _ _   _  world                =
  help "cpm module <modulename> <action>"
    [  "Where <action> is one of the following"
    ,  "  create [application|library]  : create a new module. Optionally specify module type (default: 'library')"
    //,  "  check <projectname.prj>       : type-check module in the context of project <projectname.prj>"
    //,  "  compile <projectname.prj>     : compile module in the context of project <projectname.prj>"
    ] world

/**
 * Show an error message
 */
error :: !String !*World -> *World
error message world
  # stderr     = fwrites message stderr
  # (ok,world) = fclose stderr world
  = set_return_code_world (-1) world

/**
 * Show a help message
 */
help :: !String ![String] !*World -> *World
help cmd lines world
  # lines` = [ "CPM: Clean Project Manager"
             : ""
             : "Usage: " +++ cmd
             : lines]
  = showLines lines` world

/**
 * Given a list of strings, concatenate them to a single string with newlines
 * in between, then print that new string to console.
 */
showLines :: ![String] !*World -> *World
showLines lines world
  # (console, world) = stdio world
  # console          = seqSt (\s -> fwrites (s +++ "\n")) lines console
  = snd $ fclose console world
