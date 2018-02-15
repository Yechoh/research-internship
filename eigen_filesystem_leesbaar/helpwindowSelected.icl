implementation module helpwindowSelected

import iTasks
import showCloogleResults
import extraTaskCombinators
import content
import qualified Data.Map as DM
import iTasks.UI.Editor.Builtin
import extraText
import extraList

helpwindowSelected :: String -> Task ()
helpwindowSelected filename =
		viewSharedInformation "selected" [ViewUsing (\content. mrange2text (content.selection) (content.lines)) (textArea 'DM'.newMap)] (scontentOf filename)
			>>*
			[OnAction (Action "Search shared definitions") (hasValue (\content.
				((helpwindowSelected filename
				-||
				(
					showCloogleResults (mrange2text (content.selection) (content.lines)) >>|- return ())<<@ ApplyLayout(arrangeHorizontal)))))]
    where
	mrange2text _ [] = ""
	mrange2text Nothing text = "-"
	mrange2text (Just acerange) text =
		selectText acerange text


selectText :: AceRange [String] -> String
selectText {start=(x1a,y1a),end=(x2a,y2a)} text
	#x1 = max 0 (min x1a ((length text) - 1))
	#x2 = max 0 (min x2a ((length text) - 1))
	#y1 = max 0 (min y1a ((textSize (text !! x1))))
	#y2 = max 0 (min y2a ((textSize (text !! x2))))
	= if (x1==x2)
	(subString y1 (y2-y1) (text !! x1))
	((subString y1 (textSize (text !! x1)) (text !! x1)) +++ (concat (intersperse "\n" (subset (x1+1) (x2-x1-1) text))) +++ "\n" +++ (subString 0 y2 (text !! x2) ))

vbtekst = ["Lorem Ipsum is slechts een proeftekst uit \n" ,
	"het drukkerij- en zetterijwezen. Lorem Ipsum is \n" ,
	"de standaard proeftekst in deze bedrijfstak sinds \n" ,
	"de 16e eeuw, toen een onbekende drukker een zethaak met \n" ,
	"letters nam en ze door elkaar husselde om een font-catalogus \n" ,
	"te maken. Het heeft niet alleen vijf eeuwen overleefd maar \n" ,
	"is ook, vrijwel onveranderd, overgenomen in elektronische letterzetting."]

/*de onderste zin is 74 lang*/
vbselecties :: String
vbselecties =
	(selectText {start=(-1,-1), end=(7,100)} vbtekst) +++ "\n\n\n" +++
	(selectText {start=(1,5), end=(1,5)} vbtekst) +++ "\n\n\n" +++
	(selectText {start=(1,5), end=(1,6)} vbtekst) +++ "\n\n\n" +++
	(selectText {start=(1,5), end=(2,9)} vbtekst)
