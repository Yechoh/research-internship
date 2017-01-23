implementation module messwin

import StdString, StdPSt, StdList, StdWindow, StdMisc, StdBool
import IdeState
import errwin

//--- message dialog

:: Info
	= Level1 String
	| Level2 String
	| Level3 [String]

checkDialogExistence id io
	# (st,io)	= getDialogsStack io
	= (isMember id st,io)

showInfo :: !.Info !*(PSt General) -> !*PSt General
showInfo info ps
	#!	((dlogId,_),ps)		= getInterrupt ps
	#!	((text1Id,text2Id,_),ps)
							= getInterText ps
	# (exist,ps)			= accPIO (checkDialogExistence dlogId) ps
	| not exist && not (level3 info)
		# (buttonId,ps)		= openId ps
		#!	ok				= ButtonControl "Cancel" [ControlId buttonId, ControlPos (Right,zero), ControlFunction (noLS (closeWindow dlogId))]
			content			= (texts text1Id text2Id) :+: ok
//			(size,ps)		= controlSize content False Nothing Nothing Nothing ps
		#!	(err,ps)		= openDialog Void			// modal?
									(Dialog "Messages" content
									[ WindowId dlogId
									, WindowPos (Fix,OffsetVector {vx = 200, vy = 200})
/*									, WindowPos (Fix,OffsetFun 1
										(\(dm,ps)->
											{vx = (size.w + dm.corner2.x - dm.corner1.x) >> 1
											,vy = (size.Size.h + dm.corner2.y - dm.corner1.y) >> 1
											}
										))
*/
									, WindowClose (noLS (closeWindow dlogId))
									, WindowOk buttonId
									, WindowCancel buttonId
									])
									ps
		| err == NoError
			= ps
		= ps
	#! ps = case info of
					Level1 txt	-> appPIO (setControlTexts [(text1Id,txt),(text2Id,"")]) ps
								// Can improve this by remembering if text2 == ""
					Level2 txt	-> appPIO (setControlTexts [(text2Id,txt)]) ps
					Level3 l	-> l3fun l ps
	= ps
where
	level3 (Level3 _) = True
	level3 _ = False
	
	l3fun l ps
		= updateErrorWindow l ps		// use error window for level3 messages...
	texts text1Id text2Id
		# (txt1,txt2,info) = case info of
						Level1 s -> (s,"",[])
						Level2 s -> ("",s,[])
						Level3 l -> ("","",l)
		= LayoutControl
					( ListLS
						[ TextControl txt1 [ControlWidth (PixelWidth 250),ControlPos (Left,zero),ControlId text1Id]
						, TextControl txt2 [ControlWidth (PixelWidth 250),ControlPos (Left,zero),ControlId text2Id]:
						[]]
					)
					[ ControlHMargin 0 0
					, ControlVMargin 0 0
					, ControlItemSpace 3 3
					]

closeInfo :: !*(PSt General) -> !*PSt General
closeInfo ps
	#	((pr_info,_),ps)	= getInterrupt ps
		ps					= closeWindow pr_info ps
	= ps
