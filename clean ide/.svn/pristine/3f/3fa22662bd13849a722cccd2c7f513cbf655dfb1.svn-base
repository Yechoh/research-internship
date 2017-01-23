definition module Platform

import StdString

PlatformDependant win mac :== mac

IF_MACOSX macosx not_macosx :== not_macosx

DirSeparator:==':'
DirSeparatorString:==":"

TempDir	:: String
EnvsDir		:: String
PrefsDir	:: String
BitmapDir	:: String

batchOptions	:: !*World -> (!Bool,Bool,String,*File,!*World)
wAbort			:: !String !*World -> *World

onOSX	:: Bool
