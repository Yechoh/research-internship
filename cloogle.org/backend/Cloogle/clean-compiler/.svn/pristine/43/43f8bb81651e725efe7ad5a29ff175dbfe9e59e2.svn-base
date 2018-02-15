definition module gensapl

import StdEnv,syntax,transform,backend
  
:: SaplAnnotation = SA_None | SA_Strict  
    
:: SaplExp = SaplApp SaplExp SaplExp 
		   | SaplLit SaplLiteral
           | SaplFun String 
           | SaplVar String VarInfoPtr SaplAnnotation (Optional Type) // VarInfoPtr: for comparison
           | SaplCase SaplExp [(SaplPattern,SaplExp)] (Optional SaplExp)
           | SaplLet [((SaplAnnotation,Type),SaplExp,SaplExp)] SaplExp 
           | SaplSelect SaplExp String Int
           | SaplUpdate SaplExp String [(Int, SaplExp)]
           | SaplError String 

:: SaplLiteral = LInt Int
			   | LReal Real
			   | LBool Bool
			   | LChar String
			   | LString String

:: SaplPattern = PCons String [SaplExp]
			   | PLit SaplLiteral
//			   | PDefault

// module name, type name, constructor name, ?alt?, nrargs, strictness info, nrconstructors
::SaplConsDef = SaplConsDef !String !String !String !Int !Int ![Type] !StrictnessList !Int
// fun name, nrargs, args, body, type, return type
::SaplFuncDef = SaplFuncDef !String !Int ![SaplExp] !SaplExp !FunKind !Type
// module name, type name, list of field names and global field indeces
::SaplRecordDef = SaplRecordDef !String !String !StrictnessList [(!String, !Int, !Type)] 

instance toString SaplExp
instance toString SaplConsDef
instance toString SaplFuncDef
instance toString SaplRecordDef

renameVars 		:: SaplFuncDef -> SaplFuncDef

CleanFunctoSaplFunc  :: Int CommonDefs Int Int FunDef String {#DclModule} [IndexRange] !*BackEnd !*Heaps -> *(!*BackEnd, !*Heaps, !SaplFuncDef)



