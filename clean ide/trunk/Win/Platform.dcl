definition module Platform

import StdString

PlatformDependant win mac :== win

IF_MACOSX macosx not_macosx :== not_macosx
IF_WINDOWS windows not_windows :== windows

DirSeparator:=='\\'
DirSeparatorString:=="\\"

ToolsDir	:: String
TempDir		:: String
EnvsDir		:: String
PrefsDir	:: String

batchOptions :: !*World -> (!Bool,Bool,String,*File,!*World)
wAbort :: !String !*World -> *World

onOSX	:: Bool

application_path :: !String -> String // same as applicationpath in StdSystem
