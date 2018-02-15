	implementation module saplinterface

import StdEnv, CoclSystemDependent, syntax, partition, gensapl, backend

// Generation of Sapl definition from Clean definitions
// JMJ: May 2007 
// László Domoszlai: 2011 - 

gensaplfiles :: Int {#DclModule} !{!Component} !{# FunDef} CommonDefs {#CommonDefs} Ident  [IndexRange] !*File !*BackEnd !*Heaps
                -> *(!*File, !*BackEnd, !*Heaps)
gensaplfiles main_dcl_module_n dcl_mods components fun_defs icl_common common_defs icl_name icl_function_indices sapl_file backEnd heaps
	
	# modnames = getModNames dcl_mods // modules used by this Clean module
 	# (sapl_file, backEnd, heaps) = convert2sapl main_dcl_module_n components fun_defs icl_common common_defs icl_name sapl_file modnames (toString icl_name) dcl_mods icl_function_indices backEnd heaps
	= (sapl_file, backEnd, heaps)

convert2sapl :: Int !{!Component} !{# FunDef} CommonDefs {#CommonDefs} Ident !*File [String] String {#DclModule} [IndexRange] !*BackEnd !*Heaps -> *(!*File, !*BackEnd, !*Heaps)
convert2sapl main_dcl_module_n comps fun_defs icl_common comdefs icl_name file modnames mymod dcl_mods icl_function_indices backEnd heaps
  # saplcons  = getSaplConstructors mymod icl_common        // only this module
  # extcons   = getExternalConstructors modnames comdefs     // all including this module!
  # saplrecs  = getSaplRecords  icl_common mymod
  # saplcons  = remRecs saplcons saplrecs
  # (backEnd, heaps, saplfuncs) = getSaplFunDefs main_dcl_module_n icl_common comps 0 fun_defs modnames mymod dcl_mods icl_function_indices backEnd heaps
  # saplfuncs = map renameVars saplfuncs                   // give vars a unique name
  # file = file <<< "|| ?module? " <<< mymod <<< "\n"
  # file = file <<< "\n\n"
  # file = writedef2file saplfuncs file
  # file = writedef2file (consgroups saplcons) (file <<< "\n")
  # file = writerecs2file saplrecs (file <<< "\n|| Converted Records\n")
  = (file, backEnd, heaps)
where
  consgroups conss = [":: " +++ (\ (SaplConsDef mod t _ _ _ _ _ _) = mod +++ "." +++ t) (hd a) +++ " = " +++ makeConsGroup a \\ a <- group eqcg conss] 
  eqcg (SaplConsDef a1 a2 _ _ _ _ _ _) (SaplConsDef b1 b2 _ _ _ _ _ _) = a1 == b1 && a2 == b2
  group f [] = []
  group f [x:xs] = let (as,bs) = span (f x) xs in [[x:as] : group f bs]  
  writedef2file [] file = file
  writedef2file [f:fs] file = writedef2file fs (file <<< toString f <<< "\n")
  writerecs2file [] file = file
  writerecs2file [f:fs] file = writerecs2file fs (file <<< toString f <<< "\n")
  makeString [] = ""
  makeString [f:fs] = f +++ " " +++ makeString fs
  writedeps2file [] file = file
  writedeps2file [(name,names):deps] file = writedeps2file deps (file <<< name <<< "\t\t" <<< mkString names <<< "\n") 
  mkString [] = ""
  mkString [a:as] = " " +++ a +++ mkString as
//  remRecs cs recs = [SaplConsDef mod t name alt nrargs strictness nralt \\ def =: (SaplConsDef mod t name alt nrargs strictness nralt) <- cs | not (isMember name recnames)]
  remRecs cs recs = [def \\ def =: (SaplConsDef mod t name alt nrargs _ strictness nralt) <- cs | not (isMember name recnames)]
  where 
  	recnames = [recname \\ SaplRecordDef _ recname _ _ <- recs]
  
makeConsGroup :: [SaplConsDef] -> String
makeConsGroup [     ]    = ""
makeConsGroup [arg]      = toString arg
makeConsGroup [arg:args] = toString arg +++ " | " +++ makeConsGroup args   

getSaplFunDefs :: Int CommonDefs !{!Component} !Int !{# FunDef} [String] String {#DclModule} [IndexRange] !*BackEnd !*Heaps -> *(!*BackEnd, !*Heaps, ![SaplFuncDef])
getSaplFunDefs main_dcl_module_n icl_common comps comp_index fun_defs mod_names mymod dcl_mods icl_function_indices backEnd heaps
	| comp_index >= size comps
		= (backEnd, heaps, [])
		# comp = comps.[comp_index]
		# (backEnd, heaps, saplfuncs) = show_component comp.component_members fun_defs [] backEnd heaps
		# (backEnd, heaps, sfuncs) = getSaplFunDefs main_dcl_module_n icl_common comps (inc comp_index) fun_defs mod_names mymod dcl_mods icl_function_indices backEnd heaps
		= (backEnd, heaps, saplfuncs ++ sfuncs)
where
	show_component NoComponentMembers fun_defs sapdefs backEnd heaps
		= (backEnd, heaps, sapdefs)
	show_component (ComponentMember fun funs) fun_defs sapdefs backEnd heaps
		# fun_def = fun_defs.[fun]
		# (backEnd, heaps, saplfunc) = CleanFunctoSaplFunc main_dcl_module_n icl_common comp_index fun fun_def mymod dcl_mods icl_function_indices backEnd heaps
		= show_component funs fun_defs [saplfunc:sapdefs] backEnd heaps
	show_component (GeneratedComponentMember fun _ funs) fun_defs sapdefs backEnd heaps
		# fun_def = fun_defs.[fun]
		# (backEnd, heaps, saplfunc) = CleanFunctoSaplFunc main_dcl_module_n icl_common comp_index fun fun_def mymod dcl_mods icl_function_indices backEnd heaps
		= show_component funs fun_defs [saplfunc:sapdefs] backEnd heaps

getExternalConstructors :: [String] {#CommonDefs} -> [SaplConsDef]			
getExternalConstructors mods comdefs = flatten [getSaplConstructors mod comdef\\(comdef,mod) <-  zip (lcomdefs,mods)]
where lcomdefs = [comdef\\ comdef <-: comdefs] 

getSaplConstructors :: String CommonDefs -> [SaplConsDef]			
getSaplConstructors mod icl_common = collectTypes [consdef\\ consdef <-: icl_common.com_cons_defs] 
where collectTypes conses = makeSaplCons  (group eqc (tosapl conses))
      tosapl conses = [(getConsType cons,getName cons,getNrArgs cons,getArgTypes cons,getStrictness cons) \\ cons <- conses]
      makeSaplCons conses = [SaplConsDef mod type name alt nrarg argtys sl (length css)\\ css <- conses, (alt,(type,name,nrarg,argtys,sl)) <- zip ([1..],css)]
      group f [] = []
      group f [x:xs] = let (as,bs) = span (f x) xs in [[x:as] : group f bs]
      eqc (a,_,_,_,_) (b,_,_,_,_) = a == b

	  getArgTypes cons      = (repeatn (length cons.cons_type.st_context) TE) ++ (map (\{at_type}->at_type) cons.cons_type.st_args)
      getName cons 			= toString cons.cons_ident
      getNrArgs cons 		= length cons.cons_type.st_args + length cons.cons_type.st_context
      getConsType cons 		= icl_common.com_type_defs.[cons.cons_type_index].td_ident.id_name 
	  getStrictness cons	= cons.cons_type.st_args_strictness
      
getSaplRecords :: CommonDefs String -> [SaplRecordDef]
getSaplRecords icl_common mymod = map makeRec [rectype \\ type <-: icl_common.com_type_defs, RecordType rectype <- [type.td_rhs]]
where 
	makeRec rectype = 
			SaplRecordDef mymod (toString rectype.rt_constructor.ds_ident) 
						  (icl_common.com_cons_defs.[rectype.rt_constructor.ds_index].cons_type.st_args_strictness)
						  [(toString field.fs_ident, icl_common.com_selector_defs.[field.fs_index].sd_field_nr, atype.at_type) \\ field <-: rectype.rt_fields & atype <- icl_common.com_cons_defs.[rectype.rt_constructor.ds_index].cons_type.st_args]

getModNames :: {#DclModule} -> [String]
getModNames dcl_mods = [dcl_mod.dcl_name.id_name\\ dcl_mod <-: dcl_mods]

showConstructors :: *File CommonDefs -> *File
showConstructors file icl_common = file <<< [type.rt_constructor.ds_ident\\ consdef <-: icl_common.com_type_defs, RecordType type <-  [consdef.td_rhs]]

