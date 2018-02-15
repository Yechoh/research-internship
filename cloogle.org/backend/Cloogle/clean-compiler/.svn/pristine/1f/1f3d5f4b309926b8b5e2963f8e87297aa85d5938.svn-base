implementation module gensapl

// Generation of Sapl definition from Clean definition
// JMJ: May 2007
// László Domoszlai: 2011 - 

import StdEnv, syntax, transform, backend, backendinterface, containers

genTypeInfo :: Type -> String 
genTypeInfo (TB BT_Int)  = "::I"
genTypeInfo (TB BT_Char) = "::C"
genTypeInfo (TB BT_Real) = "::D"
genTypeInfo (TB BT_Bool) = "::B"
genTypeInfo _ = ""

maybeTypeInfo :: (Optional Type) -> String
maybeTypeInfo (Yes ty) = genTypeInfo ty
maybeTypeInfo No = ""

instance toString SaplConsDef  
where 
	toString (SaplConsDef mod t name alt nrargs argtys strictness nralt) 
		= makePrintableName (mod +++ "." +++ name) 
			+++ makeString [(if (arg_is_strict (n-1) strictness) " !a" " a") +++ toString n +++ genTypeInfo argty \\ n <- [1..nrargs] & argty <- argtys]

instance toString SaplFuncDef 
where 
	toString (SaplFuncDef name nrargs args body kind mbType) 
		= makePrintableName name +++ genTypeInfo mbType +++ makeArgs args +++ toString kind +++ toString body

instance toString SaplRecordDef
where 
	toString (SaplRecordDef mod recname strictness fields) = makeGetSets mod recname strictness fields

instance toString FunKind
where 
	toString FK_Macro = " = "
	toString FK_Caf = " =: "
	toString x = " = "

instance == SaplConsDef
where 
	== (SaplConsDef _ _ name1 _ _ _ _ _) (SaplConsDef _ _ name2 _ _ _ _ _) = name1 == name2
 
// only used for comparing vars
instance == SaplExp
where 
	== var1=:(SaplVar _ _ _ _) var2=:(SaplVar _ _ _ _) = cmpvar var1 var2
    == _ _                                     		   = False

instance == SaplAnnotation
where
	== SA_None   SA_None   = True
    == SA_Strict SA_Strict = True
    == _         _         = False

instance toString SaplAnnotation
where
	toString SA_None = ""
	toString SA_Strict = "!"

makeString :: [String] -> String
makeString [] = ""
makeString [a:as] = a +++ makeString as 

fmap f (Yes a) = Yes (f a)
fmap _ No = No

toStringR :: Real -> String
toStringR r = if (r - toReal(entier r) == 0.0) (toString r +++ ".0") (toString r)

instance toString SaplLiteral
where
	toString (LInt i) 		= toString i
	toString (LReal r) 		= toStringR r
	toString (LBool b) 		= toString b
	toString (LChar c) 		= c
	toString (LString s) 	= s

instance toString SaplPattern
where
	toString (PCons name args) = makePrintableName name +++ makeString [" "+++arg\\ SaplVar arg _ _ _<- args]
	toString (PLit lit) = toString lit

instance toString SaplExp
where 
	toString e = exp2string False e
	where
		exp2string b (SaplApp left right)         = bracks b (exp2string False left +++ " " +++ exp2string True right)
		exp2string b (SaplLit l)                  = toString l
		exp2string b (SaplFun f)                  = makePrintableName f
		exp2string b (SaplVar n vi a No)          = makePrintableName n
		exp2string b (SaplVar n vi a (Yes ty))    = makePrintableName n +++ genTypeInfo ty		
		exp2string b e=:(SaplCase _ _ _)          = bracks b (caseToString e)
		exp2string b (SaplSelect expr cons idx)   = bracks b ("select " +++ exp2string True expr +++ "::" +++ cons +++ " " +++ toString idx)		
		exp2string b (SaplLet ves body)           = bracks b ("let " +++ multiLet ves body)
		exp2string b (SaplUpdate expr cons binds) = bracks b ("update " +++ exp2string True expr +++ "::" +++ cons +++ " [" +++ multiUpdate binds +++ "]")
		exp2string b (SaplError m)                = bracks b ("error \"" +++ m +++ "\"")

		bracks b e | b = "(" +++ e +++ ")" 
    		           = e
      
        caseToString :: !SaplExp -> String       
		caseToString (SaplCase e ps def) = "case " +++ exp2string True e +++ " " +++ dopats ps +++ dodef def
		where dopats [] = ""
		      dopats [(p,exp):pats] = "(" +++ toString p +++ " -> " +++ toString exp +++ ") " +++ dopats pats
		                                                 		                                                 
		      dodef No = ""
		      dodef (Yes def) = "(_ -> " +++ toString def +++ ")"
        
		multiLet :: ![((SaplAnnotation,Type),SaplExp,SaplExp)] !SaplExp -> String
		multiLet []                                body  =  toString body // empty let
		multiLet [((annotation, type), arg, e)]      body  =  toString annotation +++ toString arg +++ genTypeInfo type +++ " = " +++ toString e +++ " in " +++ toString body
		multiLet [((annotation, type), arg, e): ves] body  =  toString annotation +++ toString arg +++ genTypeInfo type +++ " = " +++ toString e +++ ", " +++ multiLet ves body

		multiUpdate [] = ""
		multiUpdate [(idx,expr)] = toString idx +++ ":" +++ toString expr 		
		multiUpdate [(idx,expr):us] = toString idx +++ ":" +++ toString expr +++ "," +++ multiUpdate us

		makeCodeString :: ![String] -> String
		makeCodeString []     = ""
		makeCodeString [c:cs] = c +++ ";" +++ makeCodeString cs 

makeArgs :: [SaplExp] -> String
makeArgs []                         = ""
makeArgs [SaplVar arg _ a mbt]      = " " +++ makePrintableAnnotatedName (toString arg) a +++ maybeTypeInfo mbt
makeArgs [SaplVar arg _ a mbt:args] = " " +++ makePrintableAnnotatedName (toString arg) a +++ maybeTypeInfo mbt +++ makeArgs args 

counterMap :: (a Int -> b) [a] Int -> [b]
counterMap f [] c = []
counterMap f [x:xs] c = [f x c : counterMap f xs (c+1)]

// Converting a single Clean function to a Sapl function (case is only pre-transformed)
CleanFunctoSaplFunc  :: Int CommonDefs Int Int FunDef String {#DclModule} [IndexRange] !*BackEnd !*Heaps -> *(!*BackEnd, !*Heaps, !SaplFuncDef)
CleanFunctoSaplFunc main_dcl_module_n icl_common modindex funindex 
                    {fun_ident,fun_body=TransformedBody {tb_args,tb_rhs},fun_info={fi_free_vars,fi_local_vars,fi_def_level,fi_calls},fun_type,fun_kind} 
                    mymod dcl_mods icl_function_indices backEnd heaps

		// Add derived strictness from backEnd
        # (backEnd, strictnessList, tupleReturn) = case fun_type of
				No = (backEnd, NotStrict, No)
        		Yes ft   
        			# (_, ft, backEnd) = addStrictnessFromBackEnd funindex fun_ident.id_name backEnd ft
					// If the return type is a strict tuple, a special name is generated for it,
					// indicating which argument is strict. Later the references to the orignal
					// Tuple constructor must be replaced by the special one (see changeTuple).
					// The necessity of the change is indicated by the Optional (from, to) value.
					# pf = case ft of 
								{st_result = {at_type = TAS ti _ (Strict x)}} 
									| startsWith "_Tuple" ti.type_ident.id_name
										= Yes (ti.type_ident.id_name, ti.type_ident.id_name+++"!"+++toString x)
										= No
								= No
        			= (backEnd, ft.st_args_strictness, pf)
	 
		// no type info yet
		# sapl_fun_args = counterMap (getFreeFuncArgName strictnessList) tb_args 0
		# (Yes symbty) = fun_type // must be
		# sapl_fun_args_typed = map (\(SaplVar name vi annot _, {at_type}) -> SaplVar name vi annot (Yes at_type)) (zip2 sapl_fun_args symbty.st_args)
		
		# (heaps, sapl_rhs) = cleanExpToSaplExp tupleReturn tb_rhs heaps
        # funDef = SaplFuncDef (mymod +++ "." +++ makeFuncName main_dcl_module_n (getName fun_ident) main_dcl_module_n funindex dcl_mods icl_function_indices mymod)
                   		       (length tb_args) sapl_fun_args_typed  
                       		   sapl_rhs fun_kind symbty.st_result.at_type
        
        = (backEnd, heaps, funDef)

where
	heapsMap f heaps ls = foldl (\(heaps,rs) x -> let (nheaps, y) = f x heaps in (nheaps, [y:rs])) (heaps,[]) (reverse ls)

	cleanExpToSaplExp :: !(Optional (String,String)) !Expression !*Heaps -> (!*Heaps, !SaplExp)
	cleanExpToSaplExp tupleReturn (Var ident) heaps = (heaps, getBoundVarName ident)
			
	cleanExpToSaplExp tupleReturn (App {app_symb, app_args, app_info_ptr}) heaps
	        = case app_symb.symb_kind of
	            SK_Generic _ kind
	                = printApplicGen app_symb kind app_args heaps  //  does not apply?
	            _   # (heaps, sapl_app_args) = heapsMap (cleanExpToSaplExp No) heaps app_args
	                = (heaps, multiApp [SaplFun (changeTuple tupleReturn app_symb) : sapl_app_args])

	cleanExpToSaplExp tupleReturn (f_exp @ a_exp) heaps 
			# (heaps, sapl_f_exp) = cleanExpToSaplExp tupleReturn f_exp heaps
			# (heaps, sapl_a_exp) = heapsMap (cleanExpToSaplExp No) heaps a_exp
			= (heaps, multiApp [sapl_f_exp: sapl_a_exp])

	cleanExpToSaplExp tupleReturn (Let {let_info_ptr, let_strict_binds, let_lazy_binds, let_expr}) heaps 
			# (heaps, sapl_let_expr) = cleanExpToSaplExp tupleReturn let_expr heaps

			# expr_heap = heaps.hp_expression_heap
			# (let_info, expr_heap) = readPtr let_info_ptr expr_heap
			# heaps = {heaps & hp_expression_heap = expr_heap}
			# btypes = case let_info of
					(EI_LetType atypes) = map (\atype -> atype.at_type) atypes

			# (heaps, sapl_bindings) = heapsMap letToSapl heaps (zip2 bindings btypes)
			= (heaps, SaplLet sapl_bindings sapl_let_expr)
	where
		bindings = zip2 (repeat SA_Strict) let_strict_binds ++ 
				   zip2 (repeat SA_None) (reverse let_lazy_binds)
		letToSapl ((annotation, binding), type) heaps 
				# (heaps, sapl_lb_src) = cleanExpToSaplExp No binding.lb_src heaps
				= (heaps, ((annotation, type), getFreeVarName binding.lb_dst, sapl_lb_src))
				
	cleanExpToSaplExp tupleReturn (Case {case_expr,case_guards,case_default,case_explicit}) heaps
			= genSaplCase case_expr case_guards case_default case_explicit heaps
	where
		// Converting Case definitions
		genSaplCase case_exp (AlgebraicPatterns gindex pats) def explicit heaps 
			# (heaps, sapl_case_exp) = cleanExpToSaplExp No case_exp heaps
			# (heaps, sapl_def) = handleDef def explicit heaps
			# (heaps, sapl_pats) = heapsMap getCasePat heaps pats 
			= (heaps, SaplCase sapl_case_exp sapl_pats sapl_def)
		genSaplCase case_exp (OverloadedListPatterns listtype exp pats) def explicit heaps 
			# (heaps, sapl_case_exp) = cleanExpToSaplExp No case_exp heaps
			# (heaps, sapl_def) = handleDef def explicit heaps
			# (heaps, sapl_pats) = heapsMap getCasePat heaps pats		
			= (heaps, SaplCase sapl_case_exp sapl_pats sapl_def)
		genSaplCase case_exp (BasicPatterns gindex pats) def explicit heaps
			# (heaps, sapl_case_exp) = cleanExpToSaplExp No case_exp heaps 
			# (heaps, sapl_def) = handleDef def explicit heaps
			# (heaps, sapl_pats) = heapsMap getConstPat heaps pats			
			= (heaps, SaplCase sapl_case_exp sapl_pats sapl_def)
		genSaplCase case_exp  _ _ _ heaps = (heaps, SaplError "no matching rule found")

		handleDef (Yes def) _ heaps	
			# (heaps, sapl_def) = cleanExpToSaplExp tupleReturn def heaps
			= (heaps, Yes sapl_def)
		handleDef _ True heaps		
			= (heaps, Yes (SaplFun "nomatch"))
		handleDef _ _ heaps			
			= (heaps, No)
		
		getConstPat pat heaps 
			# (heaps, sapl_bp_expr) = cleanExpToSaplExp tupleReturn pat.bp_expr heaps
			= (heaps, (PLit (basicValueToSapl pat.bp_value), sapl_bp_expr))
		getCasePat pat heaps 
			# (heaps, sapl_ap_expr) = cleanExpToSaplExp tupleReturn pat.ap_expr heaps
			= (heaps, (PCons (printConsName pat.ap_symbol.glob_object.ds_ident (getmodnr pat.ap_symbol))
					 		(map getFreeVarName pat.ap_vars), sapl_ap_expr))
			
	cleanExpToSaplExp tupleReturn (BasicExpr basic_value) heaps         
			= (heaps, SaplLit (basicValueToSapl basic_value))
	cleanExpToSaplExp tupleReturn (FreeVar var) heaps                             
			= (heaps, getFreeVarName var)
	cleanExpToSaplExp tupleReturn (Conditional {if_cond,if_then,if_else=No}) heaps         
			# (heaps, sapl_if_cond) = cleanExpToSaplExp No if_cond heaps 
			# (heaps, sapl_if_then) = cleanExpToSaplExp tupleReturn if_then heaps
			= (heaps, SaplCase sapl_if_cond [(PLit (LBool True), sapl_if_then)] No)
	cleanExpToSaplExp tupleReturn (Conditional {if_cond,if_then,if_else=Yes else_exp}) heaps          
			# (heaps, sapl_if_cond) = cleanExpToSaplExp No if_cond heaps 
			# (heaps, sapl_if_then) = cleanExpToSaplExp tupleReturn if_then heaps
			# (heaps, sapl_else_exp) = cleanExpToSaplExp tupleReturn else_exp heaps
			= (heaps, SaplCase sapl_if_cond [(PLit (LBool True), sapl_if_then),(PLit (LBool False), sapl_else_exp)] No)
	cleanExpToSaplExp tupleReturn (Selection _ expr selectors) heaps         
			# (heaps, sapl_expr) = cleanExpToSaplExp No expr heaps
			= makeSelector selectors sapl_expr heaps
	cleanExpToSaplExp tupleReturn (Update expr1 selections expr2) heaps                        
			# (heaps, sapl_expr1) = cleanExpToSaplExp No expr1 heaps
			# (heaps, sapl_expr2) = cleanExpToSaplExp No expr2 heaps			
			= makeArrayUpdate sapl_expr1 selections sapl_expr2 heaps
	cleanExpToSaplExp tupleReturn (RecordUpdate cons expression binds) heaps  
			# (heaps, sapl_expression) = cleanExpToSaplExp No expression heaps
			# (heaps, updates) = getUpdates binds heaps
			= (heaps, SaplUpdate sapl_expression (makePrintableName (find_rec_cons_by_cons cons)) updates)
	cleanExpToSaplExp tupleReturn (TupleSelect cons field_nr expr) heaps
			# (heaps, sapl_expr) = cleanExpToSaplExp No expr heaps		   
			= (heaps, SaplSelect sapl_expr (toString cons.ds_ident) field_nr) // TODO: strictness
	cleanExpToSaplExp tupleReturn (MatchExpr cons expr) heaps
			| cons.glob_object.ds_arity == 1 
				# (heaps, expr) = cleanExpToSaplExp No expr heaps
				= (heaps, SaplSelect expr "_Tuple1" 0) 
	            = cleanExpToSaplExp tupleReturn expr heaps
	            
	cleanExpToSaplExp _ EE heaps                                           = (heaps, SaplError "no EE")
	cleanExpToSaplExp _ (DynamicExpr {dyn_expr,dyn_type_code}) heaps       = (heaps, SaplError "no DynamicExpr")
	cleanExpToSaplExp _ (TypeCodeExpression type_code) heaps               = (heaps, SaplError "no TypeCodeExpression")
	
	cleanExpToSaplExp _ (ABCCodeExpr code_sequence do_inline) heaps        = (heaps, SaplError "no AnyCodeExpr") //SaplABCCode code_sequence
	cleanExpToSaplExp _ (AnyCodeExpr input output code_sequence) heaps     = (heaps, SaplError "no AnyCodeExpr") 
	
	cleanExpToSaplExp _ (FailExpr _) heaps                                 = (heaps, SaplError "no FailExpr") 
	
	cleanExpToSaplExp _ (ClassVariable info_ptr) heaps                     = (heaps, SaplError "ClassVariable may not occur")
	cleanExpToSaplExp _ (NoBind _) heaps                                   = (heaps, SaplError "noBind may not occur") 
	cleanExpToSaplExp _ (Constant symb _ _) heaps                          = (heaps, SaplError "Constant may not occur")
	cleanExpToSaplExp _ expr heaps                                         = (heaps, SaplError "no cleanToSapl for this case")  

	// See the comment above
	changeTuple (Yes (fromi, toi)) {symb_ident={id_name}} | fromi == id_name
		= toi
	changeTuple _ app_symb = getSymbName app_symb 	

	printApplicGen app_symb kind args heaps 
		# (heaps, sapl_args) = heapsMap (cleanExpToSaplExp No) heaps args
		= (heaps, multiApp [SaplFun (getSymbName app_symb  +++ "_generic"):sapl_args])
	                                                
	// Array and Record updates
	makeArrayUpdate expr1 sels expr2 heaps 
		# (heaps, sapl_expr1) = makeSelector sels expr1 heaps
		= (heaps, SaplApp sapl_expr1 expr2)
	
	/* 
	TODO: DictionarySelection is possibly broken. The following example (without the type) generates
	wrong code:
	
	//g :: {!e} -> [e]
	g {[0]=x} = [x]

	a :: {Int}
	a = {2}

	Start :: [Int]
	Start = g a
	*/
	
	makeSelector [] e heaps = (heaps, e)
	makeSelector [selector:sels] e heaps 
		# (heaps, sapl_expr) = mksel selector e heaps
		= makeSelector  sels sapl_expr heaps
	where mksel (RecordSelection field idx) sapl_expr heaps 
				= (heaps, SaplSelect sapl_expr (makePrintableName (find_rec_cons_by_sel field)) idx)
	      mksel (ArraySelection globsel _ e)      exp heaps 
	      		# (heaps, sapl_e) = cleanExpToSaplExp No e heaps
	      		= (heaps, multiApp [SaplFun (dcl_mods.[globsel.glob_module].dcl_name.id_name +++ "." +++ toString globsel.glob_object.ds_ident +++ "_" +++ toString globsel.glob_object.ds_index),exp, sapl_e])
	      mksel (DictionarySelection var sels _ e)exp heaps
		      	# (heaps, sapl_e) = cleanExpToSaplExp No e heaps
		      	# (heaps, sapl_sel) = makeSelector sels (getBoundVarName var) heaps
	      		= (heaps, multiApp [sapl_sel,exp,sapl_e])
	
	find_rec_cons_by_sel sel 
		  # mod = toString dcl_mods.[sel.glob_module].dcl_name		  
		  # selector_def = getSelectorDef sel.glob_module sel.glob_object.ds_index
		  # type_def = getTypeDef sel.glob_module selector_def.sd_type_index
		  # (RecordType {rt_constructor})  = type_def.td_rhs
		  # rec_var = (hd selector_def.sd_exi_vars).atv_variable		  
		  = mod +++ "." +++ toString rt_constructor.ds_ident
		  
	find_rec_cons_by_cons cons 
		  # mod = toString dcl_mods.[cons.glob_module].dcl_name		  
		  # cons_def = getConsDef cons.glob_module cons.glob_object.ds_index
		  = mod +++ "." +++ toString cons_def.cons_ident
		
	getSelectorDef glob_module index | glob_module == main_dcl_module_n
			= icl_common.com_selector_defs.[index]
			= dcl_mods.[glob_module].dcl_common.com_selector_defs.[index]			

	getConsDef glob_module index | glob_module == main_dcl_module_n
			= icl_common.com_cons_defs.[index]
			= dcl_mods.[glob_module].dcl_common.com_cons_defs.[index]			

	getTypeDef glob_module index | glob_module == main_dcl_module_n
			= icl_common.com_type_defs.[index]
			= dcl_mods.[glob_module].dcl_common.com_type_defs.[index]			

    typeName (TAS cons _ _) = toString cons.type_ident
	typeName (TA cons _) =toString cons.type_ident		
	typeName _ = "???"
	
	// backendconvert.convertSelector (BESelect)
	getUpdates [] heaps                       
			= (heaps, [])
	getUpdates [upbind:us] heaps | not (isNoBind value)
			# selector_def = getSelectorDef upbind.bind_dst.glob_module index 
			# (heaps, sapl_value) = cleanExpToSaplExp No value heaps
			# (heaps, updates) = getUpdates us heaps
			= (heaps, [(selector_def.sd_field_nr, sapl_value):updates])
			= getUpdates us heaps
	where index               = upbind.bind_dst.glob_object.fs_index
	      value               = upbind.bind_src
	      isNoBind (NoBind _) = True
	      isNoBind _          = False

	// It uses the stricness bitmap to extract annotation
	getFreeFuncArgName :: StrictnessList FreeVar Int -> SaplExp 
	getFreeFuncArgName strictness {fv_ident,fv_info_ptr,fv_count} c | arg_is_strict c strictness
                       = SaplVar (toString fv_ident) fv_info_ptr SA_Strict No
	getFreeFuncArgName strictness {fv_ident,fv_info_ptr,fv_count} c
                       = SaplVar (toString fv_ident) fv_info_ptr SA_None No
                              
	// FreeVar e.g. the name of a let binding                                  
	getFreeVarName :: FreeVar -> SaplExp 
	getFreeVarName {fv_ident,fv_info_ptr,fv_count} = SaplVar (toString fv_ident) fv_info_ptr SA_None No
	                                                                                    
	ptrToString ptr = toString (ptrToInt ptr)

	getBoundVarName {var_ident,var_info_ptr} = SaplVar (toString var_ident) var_info_ptr SA_None No
	
	// Function names at declaratio (on the left)
	getName :: Ident -> String
	getName {id_name} = id_name
	
	getSymbName :: SymbIdent -> String
	getSymbName symb=:{symb_kind = SK_Function symb_index} = printOverloaded symb.symb_ident symb_index.glob_object symb_index.glob_module
	getSymbName symb=:{symb_kind = SK_LocalMacroFunction symb_index} = printGeneratedFunction symb.symb_ident symb_index
	getSymbName symb=:{symb_kind = SK_GeneratedFunction _ symb_index} = printGeneratedFunction symb.symb_ident symb_index
	getSymbName symb=:{symb_kind = SK_LocalDclMacroFunction symb_index} = printOverloaded symb.symb_ident symb_index.glob_object symb_index.glob_module
	getSymbName symb=:{symb_kind = SK_OverloadedFunction symb_index} = printOverloaded symb.symb_ident symb_index.glob_object symb_index.glob_module
	getSymbName symb=:{symb_kind = SK_Constructor symb_index} = printConsName symb.symb_ident symb_index.glob_module
	getSymbName symb             = getName symb.symb_ident
	
	// For example: test._f3_3
	printGeneratedFunction symbol symb_index  = decsymbol (toString symbol)
	where decsymbol s                         = mymod +++ "."  +++ makeFuncName main_dcl_module_n s main_dcl_module_n symb_index dcl_mods icl_function_indices mymod
	
	// Normal case
	printOverloaded symbol symb_index modnr   = decsymbol (toString symbol)
//	where decsymbol s | startsWith "c;" s     = mymod +++ "._lc_"  +++ toString symb_index 
//	                  | startsWith "g_c;" s   = mymod +++ "._lc_"  +++ toString symb_index 
//	                                          = makemod modnr +++ makeFuncName main_dcl_module_n 0 s modnr symb_index dcl_mods icl_function_indices mymod
	where decsymbol s = makemod modnr +++ makeFuncName main_dcl_module_n s modnr symb_index dcl_mods icl_function_indices mymod

	printConsName symbol modnr
		| startsWith "_Tuple" symbstr
			= symbstr
			= makemod modnr +++ symbstr
	where
		symbstr = toString symbol
	
	getmodnr sym = sym.glob_module
	makemod n = dcl_mods.[n].dcl_name.id_name +++ "."
		
fromYes (Yes x) = x

basicValueToSapl :: BasicValue -> SaplLiteral
basicValueToSapl (BVI int)      = LInt (toInt int)
basicValueToSapl (BVInt int)    = LInt int
basicValueToSapl (BVC char)     = LChar (toSAPLString '\'' char)
basicValueToSapl (BVB bool)     = LBool bool
basicValueToSapl (BVR real)     = LReal (toReal real)
basicValueToSapl (BVS str)      = LString (toSAPLString '"' str)

cmpvar  (SaplVar n1 ip1 _ _) (SaplVar n2 ip2 _ _) | isNilPtr ip1 || isNilPtr ip2 = n1 == n2	= ip1 == ip2

getVarPrefix varname  =toString (takeWhile (\a -> a <> 'I' && a <> ';') lname)
where lname = [c\\c <-: varname]      
	
renameVars :: SaplFuncDef -> SaplFuncDef
renameVars (SaplFuncDef name nrargs args body kind mbType) 
	= SaplFuncDef name nrargs renargs (doVarRename 1 renamemap body) kind mbType
where
	renargs = [SaplVar (getVarPrefix v +++ "_" +++ toString k) ip a mbt \\ (SaplVar v ip a mbt,k) <- zip(args,[0..])]

	renamemap = renamevars args 0

	renamevars vars 0 = [(SaplVar v ip a mbt, SaplVar (getVarPrefix v +++ "_" +++ toString k) ip a No) \\ (SaplVar v ip a mbt,k) <- zip(vars,[0..])]
	renamevars vars n = [(SaplVar v ip a mbt, SaplVar (getVarPrefix v +++ "_" +++ toString n +++ "_" +++ toString k) ip a No) \\ (SaplVar v ip a mbt,k) <- zip(vars,[0..])]

	doVarRename level rens (SaplApp left right)		 = SaplApp (doVarRename level rens left) (doVarRename level rens right)
	doVarRename level rens var=:(SaplVar _ _ _ _)    = findvar var rens
	doVarRename level rens (SaplLet ves body)		 = doletrename level rens [] ves body
	doVarRename level rens (SaplCase e cases def)    = docaserename level rens e cases def
	doVarRename level rens (SaplSelect e cons idx)   = SaplSelect (doVarRename level rens e) cons idx
	doVarRename level rens (SaplUpdate e cons binds) = doupdaterename level rens e cons binds	
	doVarRename level rens e                    	 = e

	docaserename level rens e cases def = SaplCase e` (map renamecase cases) def`
	where
		e` = doVarRename level rens e
		def` = fmap (doVarRename level rens) def
	
		renamecase (PCons mexpr args, body) 
			= (PCons mexpr (map snd args`), doVarRename (level+1) (args`++rens) body)
		where
			args` = renamevars args level

		renamecase (p, body) 
			= (p, doVarRename (level+1) rens body)
	
	doupdaterename level rens e cons binds = SaplUpdate e` cons (map renamebind binds)
	where
		e` = doVarRename level rens e
		renamebind (idx, bexpr) = (idx, doVarRename level rens bexpr) 
	
	doletrename level rens _ bindings body = removeVarBodyLets (SaplLet renlets renbody)	
	where
		// extract annotations from let bindings
		annotations   = map fst3 bindings
		// apply variable renaming to vars of let bindings, bodies of let bindings and body of let
		renletvars    = renamevars (map snd3 bindings) level
		renletbodies  = [doVarRename (level+1) (renletvars++rens) b \\ (_ ,_ ,b) <- bindings]
		renbody       = doVarRename (level+1) (renletvars++rens) body
		// zip them again
		renlets 	  = [(a,rv,b) \\ a <- annotations & (v, rv) <- renletvars & b <- renletbodies]

	// Sapl does not allow let's with only a var on the right hand side
	removeVarBodyLets (SaplLet bindings body) 
		# (SaplLet bindings body) = varrename varbindings (SaplLet nonvarbindings body)
		| isEmpty bindings = body 
    	                   = SaplLet bindings body
	where
		// filter bindings by their body
		varbindings    = [(var, SaplVar n ip a No) \\ (_, var, SaplVar n ip a mbt) <- bindings]
		nonvarbindings = filter (noVar o thd3) bindings

		noVar (SaplVar _ _ _ _) = False
		noVar _                 = True

	// Simple var renaming
	varrename rens (SaplApp left right) = SaplApp (varrename rens left) (varrename rens right)
	varrename rens (SaplVar n ip a mbt) = findvar (SaplVar n ip a mbt) (rens++[(SaplVar n ip a mbt,SaplVar n ip a No)])
	varrename rens (SaplLet ves body) = SaplLet [(a,v,varrename rens e)\\ (a,v,e) <- ves] (varrename rens body)
	varrename rens (SaplCase expr patterns mbDef) 
			= SaplCase (varrename rens expr) [(p,varrename rens e)\\ (p,e) <- patterns] (fmap (varrename rens) mbDef)	
	varrename rens (SaplSelect expr cons idx)   = SaplSelect (varrename rens expr) cons idx
	varrename rens (SaplUpdate expr cons binds) = SaplUpdate (varrename rens expr) cons [(idx,varrename rens bexpr)\\ (idx,bexpr) <- binds]
	varrename rens e                     = e

findvar (SaplVar n ip a mbt) rens = hd ([renvar\\ (var,renvar) <- rens| cmpvar (SaplVar n ip a mbt) var]++[SaplVar ("error, " +++ n +++ " not found") nilPtr SA_None No])

makeFuncName main_dcl_module_n name mod_index func_index dcl_mods ranges mymod
              | name.[0] == '\\' = "anon_" +++ toString func_index
              //| startsWith "c;" name = "_lc_" +++ toString func_index
              //| startsWith "g_" name = "_lc_" +++ toString func_index
              // used for dynamic desription, there is only one per type, no need for numbering
              | startsWith "TD;" name = name
                                     = genFunctionExtension main_dcl_module_n name mod_index func_index dcl_mods ranges mymod
                                 
multiApp [a]       = a
multiApp [a:b:as]  = multiApp [(SaplApp a b): as]        

startsWith :: String String -> Bool
startsWith s1 s2 = s1 == s2%(0,size s1-1)
                                 
// Record access defintions
makeGetSets mod recname strictness fields 
	= ":: " +++ recname_pr +++ " = {" +++ makeconsargs fields +++ "}\n"  
where
	recname_pr = makePrintableName (mod +++ "." +++ recname)

	makeconsargs [     ]  		   	    = ""
	makeconsargs [(field,idx,ty)]      = annotate idx (makePrintableName (mod +++ "." +++ field)) +++ genTypeInfo ty
	makeconsargs [(field,idx,ty):args] = annotate idx (makePrintableName (mod +++ "." +++ field)) +++ genTypeInfo ty +++ ", " +++ makeconsargs args 
 
 	annotate idx name | arg_is_strict idx strictness
 		= "!" +++ name
 		= name
 	
	makeargs 0 = ""
	makeargs n = makeargs (n-1) +++ " a" +++ toString n 
 
	makerepargs _ 0 = ""
	makerepargs k n | k == n = makerepargs k (n-1) +++ " val"
     	                     = makerepargs k (n-1) +++ " a" +++ toString n

makePrintableAnnotatedName :: String SaplAnnotation -> String
makePrintableAnnotatedName f SA_None = makePrintableName f
makePrintableAnnotatedName f SA_Strict = "!" +++ makePrintableName f

makePrintableName f | ss f = "<{" +++ f +++ "}>"
                           = f
                           
where ss f = or [is_ss c \\ c <-: f]
      is_ss c = not (isAlphanum c || c == '_' || c == '.')          
	
// Which functions must be extended with a number 
genFunctionExtension :: !Int !String !Int !Int {#DclModule} [IndexRange] !String -> String
genFunctionExtension main_dcl_module_n name mod_index func_index dcl_mods ranges mymod
| mod_index == main_dcl_module_n = genFunctionExtForMain name func_index ranges
| otherwise                      = genFunctionExtForDCL name mod_index func_index dcl_mods
where
	genFunctionExtForDCL name mod_index func_index dcl_mods = gfn dcl_mods.[mod_index]
	where
	 	gfn {dcl_name, dcl_common, dcl_functions, dcl_type_funs, dcl_instances}
			=	functionName name func_index [{ir_from = 0, ir_to = dcl_instances.ir_from}, dcl_type_funs]                                                                           
	
	genFunctionExtForMain name func_index ranges = functionName name func_index ranges                                                                           
	
	functionName  name func_index ranges 
		| index_in_ranges func_index ranges
		=	name
		=	(name +++ "_" +++ toString func_index)
	
	index_in_ranges index [{ir_from, ir_to}:ranges] = (index>=ir_from && index < ir_to) || index_in_ranges index ranges
	index_in_ranges index [] = False

// Change String literal representation from ABC to SAPL (JavaScript):
//
// For ABC see scanner.icl/ScanChar:
//
// Non printable characters are converted to one of them:
// \n, \r, \f, \t, \", \' and \nnn (octal)

cb :: Int
cb =: fromChar '\b'
cv :: Int
cv =: fromChar '\v' 

toSAPLString :: Char String -> String
toSAPLString qc str = toString [qc: flatten (fromABCString (tl (init (fromString str))))]
where
	fromABCString :: [Char] -> [[Char]]
	fromABCString [] = [[qc]]
	fromABCString ['\\':chars] = let (c,chars2) = scanBSChar chars in [c: fromABCString chars2]
	fromABCString [c   :chars] = [[c]    : fromABCString chars] 

	toHex i
		| i == 0  = ['\\0']
		| i == 7  = ['\\a']		
		| i == cb = ['\\b']
		| i == cv = ['\\v']	

	toHex i = ['\\x'] ++ ['0' \\ a <- [1..2-length letters]] ++ reverse (toHex` i)
	where
		letters = reverse (toHex` i)
	
		toHex` 0 = []
		toHex` i = [hex.[i bitand 15]:toHex` (i >> 4)]  
		where
			hex = "0123456789ABCDEF" 

	scanBSChar ['\\':chars] = (['\\\\'], chars)
	// These are handled by the +1 case at the end
	//scanBSChar ['n' :chars] = (['\\n'] , chars)
	//scanBSChar ['r' :chars] = (['\\r'] , chars)
	//scanBSChar ['f' :chars] = (['\\f'] , chars)
	//scanBSChar ['t' :chars] = (['\\t'] , chars)
	//scanBSChar ['"' :chars] = (['\\"'] , chars)
	//scanBSChar ['\'':chars] = (['\\\''], chars)
	scanBSChar [f:s:t:chars] | isOctDigit f && isOctDigit s && isOctDigit t
			= (toHex (digitToInt f << 6 + digitToInt s << 3 + digitToInt t), chars)
	scanBSChar [c: chars] = (['\\',c], chars)

                                    
                     
