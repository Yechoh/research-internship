implementation module syncol

// provides preparsing for Clean syntax colouring.

import StdArray, StdClass, StdBool, StdList, StdFunc, StdString
import StrictList

/*
	parseLine: initial comment nesting level & textline -> new comment nesting level
*/

parseLine :: !.Info !.String -> Info
parseLine comment_level line
	= pL comment_level 0
where
	funnyChar c = isStringMember c (dec funnySize) funnyChars

	isStringMember :: !Char !Int !String -> Bool
	isStringMember x i s
		| i < 0 = False
		#! c = s.[i]
		| c == x = True
		= isStringMember x (dec i) s

	funnyChars	=: "~@#$%^?!+-*<>\\/|&=:."
	funnySize	= 20	// =: size funnyChars?

	line_size	= size line

	pL :: !Info !Int -> Info
	pL (level,typedef) i											// parse normal text
		| i >= line_size		= (level,typedef)
		#! char = line.[i]
		| char == '*'
			# i = inc i
			| i >= line_size	= (level,typedef)
			| line.[i] == '/'
				#! i = inc i
				| level <> 0	= pL (dec level,typedef) i			// try to fix problem below
				| i < line_size && funnyChar line.[i]
					= scanFunny (level,typedef) i					// hmmm excludes */*/ and *//*...*/
				= pL (dec level,typedef) i
			= pL (level,typedef) i
		| char == '/'
			#! i = inc i
			| i >= line_size	= (level,typedef)
			#! char = line.[i]
			| char == '/'		= (level,typedef)					// shouldn't we exclude funnyId's ??
			| char == '*'		= pL (inc level,typedef) (inc i)
			= pL (level,typedef) i
		| (char == '"') && (level == 0)
			= pS (level,typedef) (inc i)
		| (char == '\'') && (level == 0)
			= pC (level,typedef) (inc i)
		| (level == 0) && (funnyChar char)
			= scanFunny (level,typedef) i
		| i == 0 && not (WhiteSpace char)
			= pL (level,False) (inc i)
		= pL (level,typedef) (inc i)
	
	scanFunny (level=:0,typedef) 0
		|  (line_size == 2 && line == "::")
		|| (line_size >= 3 && line%(0,1) == "::" && not (funnyChar line.[2]))
			= pL (level,True) 2
		# c = line.[0]
		| typedef
			|  (line_size == 1 && (line == "|" || line == "="))
			|| (line_size >= 2 && (line%(0,0) == "|" || line%(0,0) == "=") && not (funnyChar line.[1]))
			|| (line_size == 3 && line == ":==")
			|| (line_size >= 4 && line%(0,2) == ":==" && not (funnyChar line.[3]))
				= pL (level,typedef) 1
			| funnyChar c
				= scanFunny (level,False) 1
			= pL (level,False) 0
		| funnyChar c
			= scanFunny (level,typedef) 1
		= pL (level,typedef) 0
	where
		no_c`	= line_size < 2
		c`		= line.[1]
	scanFunny (level,typedef) i
		| i >= line_size		= pL (level,typedef) line_size
		#! c = line.[i]
		| funnyChar c			= scanFunny (level,typedef) (inc i)
		= pL (level,typedef) i

	pS cl i											// parse string constant
		| i >= line_size		= cl				// unterminated string constant...
		# char = line.[i]
		| char == '"'			= pL cl (inc i)
		| char == '\\'
			= pS cl (i + 2)
		= pS cl (inc i)

	pC cl i											// parse character constant
		| i >= line_size		= cl				// unterminated char constant...
		# char = line.[i]
		| char == '\''			= pL cl (inc i)
		| char == '\\'			= pC cl (i + 2)
		= pC cl (inc i)
	
//	pT i											// parse type

WhiteSpace c
:==	c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f';

/*
	firstParse: textlines -> zip initial comment nesting level & textlines
*/
firstParse :: !(StrictList String) -> StrictList (Info,String)
firstParse lines
	= slFromList (fP (0,False) lines)
where
	fP i SNil
		= []
	fP i (SCons h t)
		#! j = parseLine i h
		= [ (i,h) : (fP j t) ]
	
/*
	quickParse: (first modified line) (last modified line) textlines
	-> last changed line with different comment nesting & textlines
*/
quickParse :: !Int !Int !(StrictList (Info,String)) -> (Int,StrictList (Info,String))
quickParse fln lln text = qP 0 text id
where
	// parse before modified
	qP :: !Int !.(StrictList (Info,String)) ((StrictList (Info,String)) -> (StrictList (Info,String))) -> (Int, (StrictList (Info,String)))
	qP cln SNil c = (cln,c SNil)
	qP cln (SCons h=:(i,l) t) c
		| cln < fln = qP (inc cln) t (SC c h)
		| cln > lln = qR (0,False) cln (SCons h t) c
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
	qR _ cln SNil c = (cln, c SNil)
	qR (level,typedef) cln r=:(SCons ((level`,typedef`),l) t) c
		| level == level` && typedef == typedef`
			= (dec cln,c r)
		# k = parseLine (level,typedef) l
		= qR k (inc cln) t (SC c ((level,typedef),l))

SC c h = \t -> c (SCons h t)
