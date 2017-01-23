/*
 * EdLook.dcl: the edit window update function
 */

definition module EdLook

from StdPicture	 import Picture
from StdIOCommon import SelectState, UpdateState, ViewFrame, UpdateArea,
						Rectangle, Point2
from StdPicture	 import FontName, FontSize, FontStyle,
						FontDef, FontMetrics, Font
//1.3
from StdString import String
//3.1
from StdPSt		 import PSt, IOSt
import EdMonad

editWindowLook	:: EditState SelectState !UpdateState -> (!*Picture -> *Picture)

// editWindowLook: defines the look of the editor window. This function
//				   is used to handle update events.

