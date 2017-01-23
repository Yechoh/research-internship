definition module ioutil

import StdOverloaded, StdBool, StdList
import StdPicture, StdPSt, StdId, StdIOCommon, StdFile, StdFileSelect
from commondef import unzip3,unzip4


instance toString FontDef
//1.3
instance accScreenPicture (PSt .l)
//3.1
/*2.0
instance accScreenPicture (PSt *l)
0.2*/
instance FileEnv Files

safeOpenFixedFont		:: !FontDef !*Picture -> (Font,*Picture);
altScrollFunction		:: !Direction !Int -> ScrollFunction
alignScrollFunction		:: !Direction !Int -> ScrollFunction
filterReturnKeys		:: KeyboardStateFilter
escFilter				:: KeyboardStateFilter
toMark					:: !Bool -> MarkState
toSelect				:: !Bool -> SelectState
noPS					:: .(.a -> .b) !(.a,.c) -> (.b,.c)
drawLeft				:: !.Point2 a !*Picture -> *Picture | toString a
drawCenter				:: !.Point2 a !*Picture -> *Picture | toString a
drawRight				:: !.Point2 a !*Picture -> *Picture | toString a
setCheckControlItem		:: !Id .Index !.Bool !*(IOSt *l) -> *(IOSt *l)
zip3					:: ![.a] [.b] [.c] -> [(.a,.b,.c)]
getPenAttributeFont		:: ![.PenAttribute] -> FontDef;
getPenAttributeColour	:: ![.PenAttribute] -> Colour;
getPenAttributeBack		:: ![.PenAttribute] -> Colour;
seqmap					:: (.a -> .(.b -> .b)) ![.a] !.b -> .b;
notEmpty s				:== not (isEmpty s)
selectDirectory`		:: !*env -> (!Maybe String,!*env) | FileSelectEnv env
