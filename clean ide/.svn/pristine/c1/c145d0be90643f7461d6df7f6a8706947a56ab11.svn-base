implementation module CpmPaths

import PmEnvironment
import System.FilePath

readIDEEnvs :: !String !String !*World -> *([Target], *World)
readIDEEnvs cleanhome ideenvs world = openEnvironments cleanhome (cleanhome </> "etc" </> ideenvs) world

