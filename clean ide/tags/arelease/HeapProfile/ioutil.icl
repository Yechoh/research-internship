implementation module ioutil

//import StdEnv, StdIO

import StdBool,StdList,StdFile
import StdControl,StdPSt
import iostate
/* zit nu in StdControl mar nog niet geexporteerd
getParentWindowId :: !Id !(IOSt .l) -> (!Maybe Id, !IOSt .l)
getParentWindowId controlId ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	  maybeParent		= getIdParent controlId idtable
	| isNothing maybeParent
		= (Nothing,ioState)
	# parent			= fromJust maybeParent
	# (ioId,ioState)	= IOStGetIOId ioState
	| ioId==parent.idpIOId && parent.idpDevice==WindowDevice
		= (Just parent.idpId,ioState)
	| otherwise
		= (Nothing,ioState)
*/
safeOpenFixedFont :: !.FontDef !*Picture -> (Font,*Picture);
safeOpenFixedFont fdef pict
	# ((ok,fnt),pict) = openFont fdef pict
	# ((ok,fnt),pict) = case ok of
							True -> ((ok,fnt),pict)
							False -> openFont fdef` pict
	| not ok
		= openDefaultFont pict
	= (fnt,pict)
where
	fdef` = NonProportionalFontDef
	
instance toString FontDef
where
	toString {fName,fSize,fStyles} = "<fName: "+++fName+++",fSize: "+++toString fSize+++",fStyles: "+++toS fStyles+++ ">"
	where
		toS [] = ""
		toS [s] = s
		toS [h:t] = h+++", "+++toS t

filterReturnKeys :: KeyboardStateFilter
filterReturnKeys = filterfun
where
	filterfun (SpecialKey key  (KeyDown False) _)	= key==enterKey
	filterfun (CharKey    '\n' (KeyDown False))		= True
	filterfun _										= False

escFilter :: KeyboardStateFilter
escFilter = filter
where
	filter (SpecialKey key KeyUp mods) = (key == escapeKey) && (mods == NoModifiers)
	filter _ = False

instance accScreenPicture (PSt .l)
where
	accScreenPicture f ps = accPIO (accScreenPicture f) ps
/*
instance Ids (PSt .l .p)
where
	openId ps = accPIO openId ps
	openIds	n ps = accPIO (openIds n) ps
	
	openRId ps = accPIO openRId ps
	openRIds	n ps = accPIO (openRIds n) ps
	
	openR2Id ps = accPIO openR2Id ps
	openR2Ids	n ps = accPIO (openR2Ids n) ps
*/
toMark :: !Bool -> MarkState
toMark True = Mark
toMark False = NoMark

toSelect :: !Bool -> SelectState
toSelect True = Able
toSelect False = Unable

noPS :: .(.a -> .b) !(.a,.c) -> (.b,.c)
noPS f (ls,ps) = (f ls,ps)

drawLeft :: !.Point2 a !*Picture -> *Picture | toString a
drawLeft point info picture
	#	text				= toString info
	=	drawAt point text picture

drawCenter :: !.Point2 a !*Picture -> *Picture | toString a
drawCenter {x,y} info picture
	#	text				= toString info
		(width,picture)		= getPenFontStringWidth text picture
	=	drawAt {x=x-width/2,y=y} text picture

drawRight :: !.Point2 a !*Picture -> *Picture | toString a
drawRight {x,y} info picture
	#	text				= toString info
		(width,picture)		= getPenFontStringWidth text picture
	=	drawAt {x=x-width,y=y} text picture

setCheckControlItem :: !.Id .Index !.Bool !*(IOSt .a) -> *IOSt .a
setCheckControlItem id idx True io = markCheckControlItems id [idx] io
setCheckControlItem id idx False io = unmarkCheckControlItems id [idx] io

zip3::![.a] [.b] [.c] -> [(.a,.b,.c)]
zip3 [a:as] [b:bs] [c:cs]	= [(a,b,c):zip3 as bs cs]
zip3 as bs cs				= []

getPenAttributeFont :: ![.PenAttribute] -> FontDef;
getPenAttributeFont [] = SansSerifFontDef
getPenAttributeFont [PenFont f:_] = getFontDef f
getPenAttributeFont [_:t] = getPenAttributeFont t

getPenAttributeColour :: ![.PenAttribute] -> Colour;
getPenAttributeColour [] = Black
getPenAttributeColour [PenColour c:_] = c
getPenAttributeColour [_:r] = getPenAttributeColour r

getPenAttributeBack :: ![.PenAttribute] -> Colour;
getPenAttributeBack [] = White
getPenAttributeBack [PenBack c:_] = c
getPenAttributeBack [_:r] = getPenAttributeBack r

instance FileEnv Files
where
	accFiles f e = f e
	appFiles f e = f e

seqmap :: (.a -> .(.b -> .b)) ![.a] !.b -> .b;
seqmap f [] e = e
seqmap f [h:t] e
	#! e = f h e
	= seqmap f t e

notEmpty s		:== not (isEmpty s)
