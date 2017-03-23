implementation module CpmLogic2

import CpmLogic

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

projectpath = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship"
pn = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship\\showlines.prj"
//pn = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223\\Libraries\\iTasks-SDK\\research internship\\call_cpm.prj"
ideenvs = "IDEEnvs"
force = False
cleanhome = "C:\\Users\\Martin\\Documents\\clean-classic-itasks-windows-x86-20161223"
//cleandir = cleanhome
pwd = cleanhome

Start :: *World -> *World
Start world = compile pn world

compile :: String *World -> *World
compile pn world
	#	(mpwd, world)  = getCurrentDirectory world
		(cpmd, world)  = accFiles GetFullApplicationPath world
    	//cleandir       = if (endsWith "bin" cpmd) (takeDirectory cpmd) cpmd
		//(cleanhome,world) = case getEnvironmentVariable "CLEAN_HOME" world of
          //             (Just ch, world)  -> (ch, world)
            //           (_, world)        -> (cleandir, world)
    = case mpwd of
      Ok pwd   -> buildProject cleanhome pwd pn world
      Error e  -> abort "Failed to read current directory"

//cpm.exe project Test create
createProject :: String *World -> *World
createProject mainmodule world = mkMainAndProject world
  where
	mkMainAndProject world
    # world = doModuleAction "" mainmodule (CreateModule ApplicationModule) world
    = mkProject world
    mkProject world
    # edit_options   = {eo={newlines=NewlineConventionUnix},pos_size=NoWindowPosAndSize}
    //Create project file using the Clean IDE libraries
    # prj            = PR_NewProject mainmodule edit_options DefaultCompilerOptions DefCodeGenOptions
                         DefApplicationOptions [!!] DefaultLinkOptions
    # project        = PR_SetRoot mainmodule edit_options DefaultCompilerOptions prj
    # projectfile    = mkProjectFile (dropExtension mainmodule)
    # (prjok, world) = accFiles (SaveProjectFile projectfile project cleanhome) world
    | not prjok      = error ("Could not create project file " +++ projectfile) world
    = world


buildProject :: String String String *World -> *World//(*Files -> ((Project,Bool,{#Char}),*Files))
buildProject cleanhome pwd pn world
  # (envs, world)            = readIDEEnvs cleanhome ideenvs world
  # proj_path                = GetLongPathName pn// = ReadProjectFile proj_path cleanhome
  # ((proj, ok, err), world) = accFiles (ReadProjectFile proj_path cleanhome) world
  | not ok || err <> ""      = error ("CPM failed while opening project: " +++ err +++ "\n") world
  # (console, world)         = stdio world
  # iniGeneral               = initGeneral True DefaultCompilerOptions cleanhome cleanhome proj envs console
  # {ls, gst_world}          = pinit force {ls=iniGeneral,gst_world=world,gst_continue_or_stop=False}
  = gst_world
  where
  pinit force_rebuild gst = BringProjectUptoDate force_rebuild cleanup gst
  cleanup exepath bool1 bool2 ps = abortLog False "" ps
