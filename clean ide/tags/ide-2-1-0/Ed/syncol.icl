implementation module syncol

// provides preparsing for Clean syntax colouring.

import StdArray, StdClass, StdBool, StdList, StdFunc, StdString
import StrictList
import StdTuple
import StdMisc
//import nodebug
//import dodebug	// StdDebug
trace_n` _ f :== f

//slFromList` :: ![a] -> StrictList a
slFromList` [] r = r
slFromList` [x:xs] r = SCons x (slFromList` xs r)

:: State = 
	{ level			:: !Int		// comment nesting level at start of line
	, typedef		:: !Bool		// in typedef at start of line
	, typedecl		:: !Bool		// in typedecl at start of line
	, offside		:: !Int		// typedecl offside level
	, parse_state	:: !ParseState
	, has_content	:: !Bool
	}

:: ParseState = StartOfBlock | CleanId | OpenPar | InfixId | Precedence | Fixity | Other

iniState
	=
		{ level			= 0
		, typedef		= False
		, typedecl		= False
		, offside		= 0
		, parse_state	= StartOfBlock
		, has_content	= False
		}

/*
	firstParse: textlines -> zip initial comment nesting level & textlines
*/
firstParse :: !(StrictList String) -> StrictList (!Info,!String)
firstParse lines
	# parsed_lines	= parse iniState lines
	= slFromList (backpatch iniState id [] parsed_lines)
where
	parse :: State (StrictList String) -> [(State,String)]
	parse state SNil
		= []
	parse state (SCons line lines)
		# state	= parseLine state line
		= [(state,line) : parse state lines]

	backpatch :: State ([(Info,String)] -> [(Info,String)]) [(Info,String)] [(State,String)] -> [(Info,String)]
	backpatch state res acc []
		= res acc
	backpatch state res acc old=:[(state`,line):lines]
		# flush	= state`.has_content || (not (state.typedecl) && state`.typedecl)
		# info	= ((state.level,state`.typedef,state`.typedecl,state`.offside,flush),line)
		| state`.has_content
			= backpatch state` (copy res acc) [info] lines
		| not (state.typedecl) && state`.typedecl
			= backpatch state` (patch res acc) [info] lines
		= backpatch state` res (accum acc info) lines

/*
	quickParse: (first modified line) (last modified line) textlines
	-> last changed line with different comment nesting & textlines
*/

quickParse :: !Int !Int !(StrictList (!Info,!String)) -> (Int,Int,StrictList (!Info,!String))
quickParse fln lln text
	// revert to firstParse until quickParse is debugged
	= (0,slLength text - 1,firstParse (slMap snd text))
/*
quickParse beg end lines
	# (s,f,l) = before 0 slFromList iniState [] lines
//	# l` = firstParse (slMap snd text)
	= trace_n` ("qP",beg,end,s,f) (s,f,l)
where
	before idx res state acc SNil
		= (0,0,res acc)
	before idx res state acc old=:(SCons (info=:(level,def,dec,off,flush),line) lines)
		| idx >= beg
			# beg`	= idx - length acc
			= during beg` beg` state res [] (slAppend (slFromList acc) old) //(acc++old)
		| flush
			= before (inc idx) res state (acc++[(info,line)]) lines
			= before (inc idx) (copy res acc) {state& level=level,typedef=def,typedecl=dec,offside=off} [(info,line)] lines
	during beg idx state res acc SNil
		= (beg,dec idx,res acc)
	during beg idx state res acc old=:(SCons (_,line) lines)
		| idx >= end
			= after beg idx state res acc old
		# state`	= parseLine state line
		# flush		= state`.has_content || (not (state.typedecl) && state`.typedecl)
		# info		= ((state.level,state`.typedef,state`.typedecl,state`.offside,state`.has_content),line)
		| state`.has_content
			= during beg (inc idx) state` (copy res acc) [info] lines
		| not (state.typedecl) && state`.typedecl
			= during beg idx {state & typedecl=True} (patch res acc) [] old
		= during beg (inc idx) state` res (acc++[info]) lines
	after beg idx state res acc SNil
		= (beg,dec idx,res acc)
	after beg idx state res acc old=:(SCons (info,line) lines)
		// kan stoppen als na flush info's gelijk zijn...
		# state`	= parseLine state line
		# flush`	= state`.has_content || (not (state.typedecl) && state`.typedecl)
		# info`		= (state.level,state`.typedef,state`.typedecl,state`.offside,flush`)
		| state`.has_content
			| eqInfo info info`
				= (beg, idx,res (acc ++ (slToList old)))
			= after beg (inc idx) state` (copy res acc) [(info`,line)] lines
		| not (state.typedecl) && state`.typedecl
			= after beg idx {state & typedecl=True} (patch res acc) [] old
		= after beg (inc idx) state` res (acc++[(info`,line)]) lines
*/
copy res acc rest
	= res (acc ++ rest)
patch res acc rest
	= res ((map (\((c,t,d,o,h),l)->((c,False,True,o,h),l)) acc) ++ rest)
accum acc info
	= acc ++ [info]

eqInfo :: !Info !Info -> Bool
eqInfo (a,b,c,d,e) (a`,b`,c`,d`,e`) = a==a` && b==b` && c==c` && d==d` && e==e`

instance == ParseState where
	(==) StartOfBlock	StartOfBlock	= True
	(==) CleanId		CleanId			= True
	(==) OpenPar		OpenPar			= True
	(==) InfixId		InfixId			= True
	(==) Precedence		Precedence		= True
	(==) Fixity			Fixity			= True
	(==) Other			Other			= True
	(==) _				_				= False
	
parseLine state=:{level,typedef, typedecl,offside,parse_state} line
	#! (index,indent,level)	= scanFirst level line
	#! (typedecl,offside`)	= if typedecl
								(if (index < line_size && indent >= 0 && indent <= offside) 
									(False,indent)
									(True,offside)
								)
								(False,if (indent >= 0) (case parse_state of
										OpenPar	-> offside
										InfixId	-> offside
										Fixity	-> offside
										Precedence	-> offside
										CleanId	-> offside
										_		-> indent
									)
									offside)
	#! parse_state			= if (indent==offside`)
								(case parse_state of
									OpenPar		-> OpenPar
									InfixId		-> InfixId
									Fixity		-> Fixity
									Precedence	-> Precedence
									CleanId		-> CleanId
									_			-> StartOfBlock
								)
								parse_state
	#! typedef				= if (index==0 && indent >= 0 && not (whiteChar line.[0])) False typedef
	#! has_content			= indent >= 0 && index < line_size
	#  not_double_colon		= line%(index,dec (scanfunny index line_size line)) <> "::"
	   not_typedecl_prefix	= parse_state == StartOfBlock
	#! has_content			= if (index>0)
								(has_content && not_double_colon && not_typedecl_prefix)
								has_content
	#! state				=	{state 
								& level=level
								, typedef=typedef
								, typedecl=typedecl
								, offside=offside`
								, parse_state=parse_state
								, has_content=has_content
								}
	= pL state index
where
	line_size
		// e.g. need to check for where and let here...
		:: Int
	line_size = size line
	
	(
	 arggh
		)
	  :: Int -> Int
	(arggh) i = i + 1

	pL state=:{level,parse_state} i								// parse normal text
		| i >= line_size		= state
		# end					= getToken level i line line_size
		# token					= line%(i,dec end)
		= case token of
			"/*"				-> pL {state & level = inc level} end	// BC
			"*/"				-> pL {state & level = dec level} end	// EC
			"//"				-> state			// LC
			"::"				-> if0				// FI special case...
									 (case parse_state of
									 	StartOfBlock
									 		| i == 0
									 				-> pL {state & typedef = True, parse_state = Other} end
									 				-> pL {state & parse_state = Other} end
									 	CleanId		-> pL {state & typedecl = True, parse_state = Other} end
									 	Fixity		-> pL {state & typedecl = True, parse_state = Other} end
									 	Precedence	-> pL {state & typedecl = True, parse_state = Other} end
									 	_			-> pL {state & parse_state = Other} end
									 )
									 (pL state end)
			"where"				-> if0
									(pL {state & parse_state = Other} end)
									(pL state end)
			"let"				-> if0
									(pL {state & parse_state = Other} end)
									(pL state end)
			"infix"				-> if0 		// LI special case...
									(if (parse_state==CleanId)
										(pL {state & parse_state = Fixity} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			"infixl"			-> if0 		// LI special case...
									(if (parse_state==CleanId)
										(pL {state & parse_state = Fixity} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			"infixr"			-> if0 		// LI special case...
									(if (parse_state==CleanId)
										(pL {state & parse_state = Fixity} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			"("					-> if0	// OP .. CP
									(if (parse_state==StartOfBlock)
										(pL {state & parse_state = OpenPar} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			")"					-> if0	// CP
									(if (parse_state==InfixId)
										(pL {state & parse_state = CleanId} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
			_
							| isDigit line.[i]
								-> if0
									(if (parse_state==Fixity)
										(pL {state & parse_state = Precedence} end)
										(pL {state & parse_state = Other} end)
									)
									(pL state end)
							| isLower line.[i] || isUpper line.[i] || funnyChar line.[i]
								-> if0
									(if (parse_state==StartOfBlock)
										(pL {state & parse_state = CleanId} end)
										(if (parse_state==OpenPar)
											(pL {state & parse_state = InfixId} end)
											(pL {state & parse_state = Other} end)
										)
									)
									(pL state end)
							| whiteChar line.[i]
								->	(pL state end)
							// otherwise
								->	if0
										(pL {state & parse_state = Other} end)
										(pL state end)
	where
		if0 t f
			| level==0	= t
						= f
		proceed si so se
			| parse_state == si	= so
								= se

// rework scanFirst to use getToken?!
scanFirst :: !Int !.String -> (!Int,!Int,!Int)
scanFirst level line = scanFirst level 0 0 line
where
	line_size	= size line

	scanFirst :: !Int !Int !Int !.String -> (!Int,!Int,!Int)
	// commentlevel index indent line -> (first_index,first_indent,commentlevel)
	scanFirst level index indent line
		| index >= line_size	= (index,-1,level)
		# char = line.[index]
		| char == ' '			= scanFirst level (inc index) (inc indent) line
		| char == '\t'			= scanFirst level (inc index) ((inc (indent >> 2)) << 2) line	// assumes tab=4
		| char == '\n'			= scanFirst level (inc index) indent line
		| char == '\r'			= scanFirst level (inc index) indent line
		| char == '\f'			= scanFirst level (inc index) indent line
		
		| char == '*'
			# index` = inc index
			  indent` = inc indent
			| index` >= line_size	= (index,indent,level)
			| line.[index`] == '/'
				# index`` = inc index`
				  indent`` = inc indent`
				| level <> 0	= scanFirst (dec level) index`` indent`` line	// try to fix problem below
				| index`` >= line_size	= (index``,-1,dec level)
				| funnyChar line.[index``]
					= (index,indent,level)					// hmmm excludes */*/ and *//*...*/
				= scanFirst (dec level) index`` indent`` line
			| level == 0
				= (index,indent,level)
			= scanFirst level index` indent` line
		| char == '/'
			# index` = inc index
			  indent` = inc indent
			| index` >= line_size	= (index,indent,level)
			# char` = line.[index`]
			| char` == '/'		= (index,-1,level)					// shouldn't we exclude funnyId's ??
			| char` == '*'		= scanFirst (inc level) (inc index`) (inc indent`) line
			| level == 0
				= (index,indent,level)
			= scanFirst level index` indent` line
		| level <> 0
			= scanFirst level (inc index) (inc indent) line
		= (index,indent,level)

////////////

isStringMember :: !Char !Int !String -> Bool
isStringMember x i s
	| i < 0 = False
	#! c = s.[i]
	| c == x = True
	= isStringMember x (dec i) s

funnyChar c = isStringMember c (dec funnySize) funnyChars
where
	funnyChars	=: "~@#$%^?!+-*<>\\/|&=:."
	funnySize	= 20	// =: size funnyChars?

scanfunny :: !Int !Int !String -> Int
scanfunny i line_size line
	| i >= line_size = line_size
	| funnyChar line.[i] = scanfunny (inc i) line_size line
	= i

cleanChar c = isLower c || isUpper c || isDigit c || c == '_' || c == '`'

scanclean :: !Int !Int !String -> Int
scanclean i line_size line
	| i >= line_size = line_size
	| cleanChar line.[i] = scanclean (inc i) line_size line
	= i

whiteChar c = isStringMember c (dec whiteSize) whiteChars
where
	whiteChars	=: " \t\f\n\r"
	whiteSize	= 5

scanwhite :: !Int !Int !String -> Int
scanwhite i line_size line
	| i >= line_size = line_size
	| whiteChar line.[i] = scanwhite (inc i) line_size line
	= i

getToken :: !Int !Int !String !Int -> Int
getToken level index line line_size
	| index >= line_size	= line_size
	#! char					= line.[index]
	#! i = inc index
	| char == '*'
		| i >= line_size
			= line_size
		| line.[i] == '/'
			#! i = inc i
			| level <> 0
				= i
			= scanfunny i line_size line
		= scanfunny i line_size line
	| char == '/'
		| i >= line_size
			= line_size
		#! char = line.[i]
		   i	= inc i
		| char == '/'
			= i
		| char == '*'
			= i
		= scanfunny i line_size line
	| (char == '"') && (level == 0)
		= pS i
	| (char == '\'') && (level == 0)
		= pC i
	| (level == 0) && (funnyChar char)
		= scanfunny i line_size line
	| isLower char || isUpper char
		= scanclean i line_size line
	| whiteChar char
		= scanwhite i line_size line
	= i
where
	pS i										// parse string constant
		| i >= line_size		= line_size				// unterminated string constant...
		# char = line.[i]
		| char == '"'			= (inc i)
		| char == '\\'			= pS (i + 2)
		= pS (inc i)

	pC i										// parse character constant
		| i >= line_size		= line_size				// unterminated char constant...
		# char = line.[i]
		| char == '\''			= (inc i)
		| char == '\\'			= pC (i + 2)
		= pC (inc i)
