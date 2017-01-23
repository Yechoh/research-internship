implementation module ioutil

import StdBool,StdList,StdFile
import StdControl,StdPSt,StdFileSelect
import iostate

/*2.0
// Hack around temp oio20+unique version...
returnKey :== enterKey
0.2*/

altScrollFunction :: !Direction !Int -> ScrollFunction
altScrollFunction direction d
	= altScrollFunction` direction d
where
	altScrollFunction` :: !Direction !Int !ViewFrame !SliderState !SliderMove -> Int
	altScrollFunction` direction d {corner1,corner2} {sliderThumb=x} move
		# d				= abs d
		  edge			= if (direction==Horizontal)
		  					(abs (corner2.x-corner1.x-d))
		  					(abs (corner2.y-corner1.y-d))
		= case move of
			SliderIncSmall	-> x+d
			SliderDecSmall	-> x-d
			SliderIncLarge	-> x+edge
			SliderDecLarge	-> x-edge
			SliderThumb x	-> x

alignScrollFunction :: !Direction !Int -> ScrollFunction
alignScrollFunction direction d
	= alignScrollFunction` direction d
where
	alignScrollFunction` :: !Direction !Int !ViewFrame !SliderState !SliderMove -> Int
	alignScrollFunction` direction d {corner1,corner2} {sliderThumb=x,sliderMax=m} move
		# d				= abs d
		  edge			= if (direction==Horizontal)
		  					(abs (corner2.x-corner1.x-d))
		  					(abs (corner2.y-corner1.y-d))
		= case move of
			SliderIncSmall	-> let x` = x+d in if (x`>m) m (align x`)
			SliderDecSmall	-> align (x-d)
			SliderIncLarge	-> align (x+edge)
			SliderDecLarge	-> align (x-edge)
			SliderThumb x	-> if (x == m) x (align x)
	align x = (x / d) * d

safeOpenFixedFont :: !FontDef !*Picture -> (Font,*Picture);
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
	filterfun (SpecialKey key  (KeyDown False) _)	= key==enterKey || key==returnKey
//	filterfun (CharKey    '\n' (KeyDown False))		= True
	filterfun _										= False

escFilter :: KeyboardStateFilter
escFilter = filter
where
	filter (SpecialKey key KeyUp mods) = (key == escapeKey) && (mods == NoModifiers)
	filter _ = False

//1.3
instance accScreenPicture (PSt .l)
//3.1
/*2.0
instance accScreenPicture (PSt *l)
0.2*/
where
	accScreenPicture f ps = accPIO (accScreenPicture f) ps

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

setCheckControlItem :: !Id .Index !.Bool !*(IOSt *l) -> *(IOSt *l)
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

selectDirectory` :: !*env -> (!Maybe String,!*env) | FileSelectEnv env
selectDirectory` env
//	= selectDirectory Nothing env
	# initial = global.[0]	// (Just initial_dir)
	# (result,env) = selectDirectory initial env
	# (result,_) = case result of
					Nothing -> (result,global)
					(Just _) -> update_maybe_string result global
	= (result,env)

//== UNSAFE HACK...

import StdArray

global =: {Just ""}

//update_maybe_string :: !(Maybe String) !*{(Maybe String)} -> (!(Maybe String),!*{(Maybe String)})
update_maybe_string :: !(Maybe String) !{(Maybe String)} -> (!(Maybe String),!{(Maybe String)})
update_maybe_string ms ar
//	= (ms,{ar & [0] = ms})
	= code {
		push_a 0
		pushI 0
		push_a 2
		update_a 2 3
		update_a 1 2
		updatepop_a 0 1
		update _ 1 0
		push_a 1
		update_a 1 2
		updatepop_a 0 1
	}
