implementation module syncol

/*
	syncol: provides preparsing for Clean syntax colouring.
*/

import StdArray, StdClass, StdBool, StdList, StdFunc, StdString
import StrictList

/*
	parseLine: initial comment nesting level & textline -> new comment nesting level
*/

parseLine :: !.Int !.String -> Int
parseLine comment_level line
	= pL comment_level 0
where
	funnyChar c = isStringMember c (dec funnySize) funnyChars

	//isStringMember:: a !.[a] -> .Bool | Eq a
	isStringMember :: !Char !Int !String -> Bool
	isStringMember x i s
		| i < 0 = False
		#! c = s.[i]
		| c == x = True
		= isStringMember x (dec i) s

	funnyChars	=: "~@#$%^?!+-*<>\\/|&=:."
	funnySize	= 20	// =: size funnyChars?

	line_size	= size line

	pL :: !Int !Int -> Int
	pL l i											// parse normal text
		| i >= line_size		= l
		#! char = line.[i]
		| char == '*'
			# i = inc i
			| i >= line_size	= l
			| line.[i] == '/'
				#! i = inc i
				| l <> 0		= pL (dec l) i		// try to fix problem below
				| i < line_size && funnyChar line.[i]
					= scanFunny l i					// hmmm excludes */*/ and *//*...*/
				= pL (dec l) i
			= pL l i
		| char == '/'
			#! i = inc i
			| i >= line_size	= l
			#! char = line.[i]
			| char == '/'		= l					// shouldn't we exclude funnyId's ??
			| char == '*'		= pL (inc l) (inc i)
			= pL l i
		| (char == '"') && (l == 0)
			= pS (inc i)
		| (char == '\'') && (l == 0)
			= pC (inc i)
		| (l == 0) && (funnyChar char)
			= scanFunny l i
		= pL l (inc i)
	
	scanFunny l i
		| i >= line_size		= pL l line_size
		#! c = line.[i]
		| funnyChar c			= scanFunny l (inc i)
		= pL l i

	pS i											// parse string constant
		| i >= line_size		= 0					// unterminated string constant...
		# char = line.[i]
		| char == '"'			= pL 0 (inc i)
		| char == '\\'
			= pS (i + 2)
		= pS (inc i)

	pC i											// parse character constant
		| i >= line_size		= 0					// unterminated char constant...
		# char = line.[i]
		| char == '\''			= pL 0 (inc i)
		| char == '\\'			= pC (i + 2)
		= pC (inc i)
	
//	pT i											// parse type

/*
	firstParse: textlines -> zip initial comment nesting level & textlines
*/
firstParse :: !(StrictList String) -> (Int, StrictList (Int,String))
firstParse lines
	= fP 0 lines
where
	fP i SNil
		= (i,SNil)
	fP i (SCons h t)
		#! j = parseLine i h
		# (k,r) = fP j t
		= (k,SCons (i,h) r)

/*
	quickParse: (first modified line) (last modified line) textlines
	-> last changed line with different comment nesting & textlines
*/
quickParse :: !Int !Int !(StrictList (Int,String)) -> (Int,StrictList (Int,String))
quickParse fln lln text = qP 0 text id
where
	// parse before modified
	qP :: !Int !.(StrictList (Int,String)) ((StrictList (Int,String)) -> (StrictList (Int,String))) -> (Int, (StrictList (Int,String)))
	qP cln SNil c = (cln,c SNil)
	qP cln (SCons h=:(i,l) t) c
		| cln < fln = qP (inc cln) t (SC c h)
		| cln > lln = qR 0 cln (SCons h t) c
		#! k = parseLine i l
		| cln < lln
			=  qS k (inc cln) t (SC c h)
		= qR k (inc cln) t (SC c h)
		
	// parse modified section
	qS i cln SNil c = (cln,c SNil)
	qS i cln (SCons h=:(_,l) t) c
		#! k = parseLine i l
		| cln < lln
			= qS k (inc cln) t (SC c (i,l))
		= qR k (inc cln) t (SC c (i,l))
	
	// parse after modified
	qR k cln SNil c = (cln, c SNil)
	qR k cln r=:(SCons (i,l) t) c
		| k == i = (dec cln,c r)
		# d = k - i
		= (cln+slLength t,c (slMap (\(i,l) -> (i+d,l)) r))

SC c h = \t -> c (SCons h t)

//--
/*
	Extension to datatype definities simple...
	Need to replace Int by alg datatype
	int now indicates comment nesting level...
	need to differentiate is/isn't dtd
	Normal Int | DTD Int
	then check in column 0 if it says '::'...
*/
