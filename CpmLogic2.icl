module CpmLogic2

import CpmLogic

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

//doCpmAction cleanhome  pwd  (Project pn pa)   world 
//	= doProjectAction cleanhome pwd pn pa world

pn = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship\\call_cpm.prj"

Start :: *World -> *World
Start world
	#(mpwd, world)  = getCurrentDirectory world
	 (cleanhome,world) = case getEnvironmentVariable "CLEAN_HOME" world of
                       (Just ch, world)  -> (ch, world)
                       (_, world)        -> (cleandir, world)
	= case mpwd of
		Ok pwd -> createProject cleanhome pwd pn world
		Error e -> abort "Failed to read current directory"

createProject :: String String String *World -> *World
createProject cleanhome pwd pn world
  # (exists,world)  = fileExists mainmodule world
  | not exists
    # world         = showLines ["Main module " +++ mainmodule +++ " does not exist. Create it? [y/n]"] world
    # (line, world) = getLine world
    | line.[0] == 'y' = mkMainAndProject world
    | otherwise       = error ("Failed to create project. Need " +++ mainmodule) world
  | otherwise       = mkProject world
  where
  basefilename = dropExtension pn
  mainmodule   = addExtension basefilename "icl"
