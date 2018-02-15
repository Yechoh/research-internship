implementation module helpwindowImports

import iTasks
import extraTaskCombinators
import content
import dcls
import extraText
import directoryBrowsing
import mapshare
import shares
import iTasks.Extensions.Editors.Ace

helpwindowImports :: String -> Task ()
helpwindowImports a = return ()
/*helpwindowImports filename =
	(dclOf filename >>- \dcl.
	enterChoiceWithShared "click to show overview of imported file" [ChooseFromGrid snd3] (mapRead (filter (\a.startsWith "import " (snd3 a))) (msgets filename dclStore))
	>&> withSelection (viewInformation "" [] "") (\(sharenum,line,comment).
		filenameToFilepath ((dropChars 7 line)+++".icl") >>- \mfp.
			case mfp of
				Nothing = viewInformation "" [] "cannot find path" >>|- return ()
				Just filepath =
					readLinesFromFile filepath >>- \mlines.
					case mlines of
						Nothing = (viewInformation "" [] "cannot read file" >>|- return ())
						Just lines =
							enterChoice "click to go to file" [ChooseFromGrid snd3] (readDcl lines)
							>&> withSelection (return ()) (\(sharenum,line,comment).
								setContents filepath  >>|-
								contentOf filepath >>- \content.
								jumpToLine filepath (indexOf line content.lines))
		>>|- return () ))<<@ (ApplyLayout arrangeHorizontal)
*/
