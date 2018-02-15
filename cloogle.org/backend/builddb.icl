implementation module builddb

import StdArray
import StdBool
import StdFile
from StdFunc import const, o
import StdList
import StdMisc
import StdString
import StdTuple

import Data.Either
from Data.Func import $, mapSt
import Data.Functor
import Data.Maybe
import System.CommandLine
from Text import class Text(concat,startsWith), instance Text String

import directoryBrowsing
import System.Directory
import Data.Error
import System.FilePath
import System.Environment
import StdFile, System.File

import CloogleDB
import Type
from CloogleDBFactory import :: TemporaryDB, newTemporaryDb, finaliseDb,
	findModules, indexModule, constructor_functions, record_functions

:: CLI = { help    :: Bool
         , version :: Bool
         , root    :: String
         , libs    :: [(String, String -> Bool)]
         , exclude :: [String]
         }

instance zero CLI where
	zero = { version = False
	       , help    = False
	       , root    = "/opt/clean/lib/"
	       , libs    = [ ("ArgEnv", const False)
	                   , ("CleanInotify", const False)
	                   , ("CleanPrettyPrint", const False)
	                   , ("CleanSerial", const False)
	                   , ("CleanSnappy", const False)
	                   , ("CleanTypeUnifier", const False)
	                   , ("Cloogle", const False)
	                   , ("Directory", const False)
	                   , ("Dynamics", const False)
	                   , ("Gast", const False)
	                   , ("Generics", const False)
	                   , ("GraphCopy", const False)
	                   , ("MersenneTwister", const False)
	                   , ("ObjectIO", not o startsWith "Std")
	                   , ("Platform", const False)
	                   , ("Sapl", const False)
	                   , ("SoccerFun", const False)
	                   , ("StdEnv", const False)
	                   , ("StdLib", const False)
	                   , ("TCPIP", const False)
	                   , ("iTasks", const False)
	                   , ("libcloogle", const False)
	                   ]
	       , exclude = [ "StdEnv/_startup"
	                   , "StdEnv/_system"
	                   , "Platform/Deprecated"
	                   , "Platform/Data/Graph/Inductive/Query"
	                   , "SoccerFun/RefereeCoach_"
	                   , "SoccerFun/Team_"
	                   , "Cloogle/compiler-patch"
	                   ]
	       }


VERSION :== "Cloogle's builddb version 0.1\n"
USAGE :== concat [
	VERSION, "\n",
	"Usage: ./builddb [opts] > types.json\n\n",
	"\t-h, --help Show this help\n",
	"\t-r PATH    Change the library root to PATH\n",
	"\t-l PATH    Add PATH to the librarypaths relative to the root\n"]

buildit :: *World -> (CloogleDB,*World)
buildit w
//# (args, w) = getCommandLine w
# (res, w)	= 	getCurrentDirectory w
| isError res	= 	abort "no pwd"
# pwd			= 	fromOk res
# (res,w) = getEnvironmentVariable "CLEAN_HOME" w
| isNothing res = abort "no cleanhome"
# cleanhome = fromJust res
# (f, w) = stdio w
# (dirs, w)	 = fetchDirectories (cleanhome</>"lib") w
# (libs)	 = map (\dir. (dropDirectory dir,(\a.False))) dirs
# (modss, w)  = mapSt (flip (uncurry $ findModules [] (cleanhome</>"lib")) "") libs w
# mods        = flatten modss
# (db, w)    = loop (cleanhome</>"lib") mods newTemporaryDb w
# db         = finaliseDb db newDb
# db         = putFunctions predefFunctions db
# db         = putClasses predefClasses db
# db         = putTypes predefTypes db
# db         = putFunctions (flatten $ map constructor_functions predefTypes) db
# db         = putFunctions (flatten $ map record_functions predefTypes) db
# (ok1,w)    = fclose (printStats db stderr dirs) w
# f          = saveDb db f
# (ok2,w)    = fclose f w
| not ok1 /*|| not ok2*/ = abort "Couldn't close stdio"
= (db,w)

fetchDirectories ::  FilePath *World -> ([FilePath],*World)
fetchDirectories pwd w
	#(fnames,w) = 		readDir pwd w
	#(dirs,nodirs,w) = 	selectDirs pwd (filter (\a. a<> "." && a<> "..") fnames) w
	=					(dirs,w)//fetch pwd dirs w
where
  fetch pwd [] w
	= 						([],w)
  fetch pwd [d:ds] w
	# (dir,w) =				fetchDirectories (pwd </> d) w
	# (dirs,w) =			fetch pwd ds w
    =  (dir++dirs,w)

selectDirs :: FilePath [FileName] *World -> ([FileName],[FileName],*World)
selectDirs pwd paths w = selDirs` paths ([],[]) w
where
	selDirs` [] (dirs,nodirs) w
		= (dirs,nodirs,w)
	selDirs` [f:fs] (dirs,nodirs) w
		# (b,w) = isDirectory (pwd </> f) w
		= if b
		 	(selDirs` fs ([f:dirs],nodirs) w)
			(selDirs` fs (dirs,[f:nodirs]) w)

isDirectory :: FilePath *World -> (Bool,*World)
isDirectory file world
	# (res,world) 	=	getFileInfo file world
	| isError res	=	abort "Cannot get directory info from file: "
	= ((fromOk res).directory,world)

readDir :: FilePath *World -> ([FileName], *World) // returns file names (no path)
readDir pwd  world
	# (res,	world)		= readDirectory pwd world
	| isError res		= abort "Cannot read directory: "
	# names				= fromOk res
	= (names, world)		// includes "." and ".."


loop :: String [(String,String,Bool)] !TemporaryDB !*World -> *(!TemporaryDB, !*World)
loop _ [] db w = (db,w)
loop root [(lib,mod,iscore):list] db w
#! w = snd (fclose (stderr <<< lib <<< ": " <<< mod <<< "\n") w)
#! (db, w) = indexModule root mod lib iscore db w
#! db = eval_all_nodes db
= loop root list db w

eval_all_nodes :: !.a -> .a // From GraphCopy
eval_all_nodes g = code {
	push_a 0
	.d 1 0
	jsr	_eval_to_nf
	.o 0 0
	}

parseCLI :: [String] -> Either String CLI
parseCLI [] = Right zero
parseCLI [x:a] = case (x,a) of
	("--help", xs) = (\c->{c & help=True}) <$> parseCLI xs
	("--version", xs) = (\c->{c & version=True}) <$> parseCLI xs
	("-l", []) = Left "'-l' requires an argument"
	("-r", []) = Left "'-r' requires an argument"
	("-r", [x:xs]) = (\c->{c & root=x}) <$> parseCLI xs
	("-l", [x:xs]) = (\c->{c & libs=[(x,const False):c.libs]}) <$> parseCLI xs
	(x, _) = Left $ "Unknown option '" +++ x +++ "'"

printStats :: !CloogleDB !*File [String] -> *File
printStats db f dirs = f
	<<< "+-------------+-------+\n"
	<<< "| Modules     | " <<< modules <<< " |\n"
	<<< "| Functions   | " <<< funs    <<< " |\n"
	<<< "| Types       | " <<< types   <<< " |\n"
	<<< "| Classes     | " <<< classes <<< " |\n"
	<<< "| Derivations | " <<< derives <<< " |\n"
	<<< "+-------------+-------+\n"
	<<< (concat dirs)///(foldr (<<<" - "<<<) ("") dirs)
	where
		[modules,funs,types,classes,derives:_]
			= map (pad 5)
				[ moduleCount db
				, functionCount db
				, typeCount db
				, classCount db
				, deriveCount db
				]
		pad n i = {' ' \\ _ <- [0..n-size (toString i)-1]} +++ toString i

predefFunctions :: [(Location, FunctionEntry)]
predefFunctions
	= [ ( Builtin "if"
	    , {zero & fe_type=Just $ Func [Type "Bool" [], Var "a", Var "a"] (Var "a") []}
	    )
	  , ( Builtin "dynamic"
	    , {zero & fe_type=Just $ Func [Var "a"] (Type "Dynamic" []) []}
	    )
	  ]

predefClasses :: [(Location, ClassEntry)]
predefClasses
	= [ ( Builtin "TC"
	    , { ce_vars=["v"]
	      , ce_context=[]
	      , ce_documentation=Nothing
	      , ce_members=[]
	      , ce_instances=[]
	      }
	    )
	  ]

predefTypes :: [(Location, TypeDefEntry)]
predefTypes
	= [ ( Builtin "Bool"
	    , { deft
	      & tde_typedef.td_name = "Bool"
	      , tde_typedef.td_rhs  = TDRCons False
	        [ { defc & cons_name="False" }
	        , { defc & cons_name="True" }
	        ]
	      }
	    )
	  , ( Builtin "Int",     { deft & tde_typedef.td_name = "Int"     } )
	  , ( Builtin "Real",    { deft & tde_typedef.td_name = "Real"    } )
	  , ( Builtin "Char",    { deft & tde_typedef.td_name = "Char"    } )
	  , ( Builtin "String",  { deft & tde_typedef.td_name = "String",
	      tde_typedef.td_rhs = TDRSynonym (Type "_#Array" [Type "Char" []]) } )
	  , ( Builtin "Dynamic", { deft & tde_typedef.td_name = "Dynamic" } )
	  , ( Builtin "File",    { deft & tde_typedef.td_name = "File"    } )
	  , ( Builtin "World",   { deft & tde_typedef.td_name = "World",
	      tde_typedef.td_uniq = True } )
	  ]
where
	deft = {tde_typedef={td_name="", td_uniq=False, td_args=[], td_rhs=TDRAbstract}, tde_doc=Nothing}
	defc = {cons_name="", cons_args=[], cons_exi_vars=[], cons_context=[], cons_priority=Nothing}
