definition module ioutil

import StdOverloaded, StdBool, StdList
import StdPicture, StdPSt, StdId, StdIOCommon, StdFile
from commondef import unzip3,unzip4

//getParentWindowId :: !Id !(IOSt .l) -> (!Maybe Id, !IOSt .l)

safeOpenFixedFont :: !.FontDef !*Picture -> (Font,*Picture);

instance toString FontDef
instance accScreenPicture (PSt .l)
//instance Ids (PSt .l .p)
instance FileEnv Files

filterReturnKeys :: KeyboardStateFilter
escFilter :: KeyboardStateFilter
toMark :: !Bool -> MarkState
toSelect :: !Bool -> SelectState
noPS :: .(.a -> .b) !(.a,.c) -> (.b,.c)
drawLeft :: !.Point2 a !*Picture -> *Picture | toString a
drawCenter :: !.Point2 a !*Picture -> *Picture | toString a
drawRight :: !.Point2 a !*Picture -> *Picture | toString a
setCheckControlItem :: !.Id .Index !.Bool !*(IOSt .a) -> *IOSt .a;
zip3::![.a] [.b] [.c] -> [(.a,.b,.c)]
//unzip3 ::![(.a,.b,.c)] -> ([.a],[.b],[.c]) // now in ObjectIO/commondef
getPenAttributeFont :: ![.PenAttribute] -> FontDef;
getPenAttributeColour :: ![.PenAttribute] -> Colour;
getPenAttributeBack :: ![.PenAttribute] -> Colour;
seqmap :: (.a -> .(.b -> .b)) ![.a] !.b -> .b;
notEmpty s		:== not (isEmpty s)
