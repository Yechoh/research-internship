definition module morecontrols

//1.3
from StdString import String
//3.1
from StdIOBasic import IdFun, Index
from StdControlDef import ControlAttribute, PopUpControl, PopUpControlItem
from StdClass import toString,==
import StdControlClass

:: FontNameSizeControl ls pst =
  FontNameSizeControl
	.String
	.Int
	[.String]
	[.Int]
	(String -> IdFun *(ls,pst))
	(Int -> IdFun *(ls,pst))
	[.ControlAttribute *(ls,pst)]

instance Controls FontNameSizeControl
