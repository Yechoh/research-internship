
#define BIT(n)			((BITVECT) 1 << n)
#define ALLBITSCLEAR	((BITVECT) 0)	
#define ALLBITSSET		(~ALLBITSCLEAR)
#define BITTEST(v,n)	(((BITVECT) v >> n) & ((BITVECT) 1))
#define TCONS_BIT_NR	31

extern Ident AnnotatedId, ListId, TupleId, ConsId, NilId, ApplyId, SelectId, IfId, FailId, StdBoolId,
			 AndId, OrId, StdArrayId, ArrayFunctionIds [], ArrayId, StrictArrayId, UnboxedArrayId, ArrayClassId;
#if STRICT_LISTS
extern Ident StrictListId,UnboxedListId,TailStrictListId,StrictTailStrictListId,UnboxedTailStrictListId;
#endif
#ifdef CLEAN2
extern Ident DynamicId;
#endif
#if SA_RECOGNIZES_ABORT_AND_UNDEF
extern Ident StdMiscId,abort_id,undef_id;
#endif
extern Ident PreludeId,seq_id,system_seq_id;

extern Symbol StartSymbol, UnboxedArrayClassSymbols [], UnboxedArrayFunctionSymbols [];
extern SymbDef scc_dependency_list,ArrayFunctionDefs[], StdArrayAbortDef;
extern char * CurrentDefModule;

extern int rule_count;
extern SymbDef *scc_dependency_list_p;

SymbDef BuildNewSymbolDefinition (Ident sid,int arity,SDefKind kind,unsigned line_nr);
SymbDef MakeNewSymbolDefinition (char * module, Ident name, int arity, SDefKind kind);
SymbDef NewSymbolDefinition (Symbol symb, int arity, Bool maybedefined, unsigned line_nr);
char *ConvertSymbolToString (Symbol symb);
ImpMod ParseAndCheckImplementationModule (char *name);
void ReadInlineCode (void);
void InitChecker (void);
void GenDependencyList (void);
NodeDefs NewNodeDef (NodeId nid, Node node);
void GenerateApplyNodesForFullyCurriedApplication (Node node, Node function_node);

void DetermineRuleComponent (ImpRules rule,SymbDef sdef);
NodeP DetermineGraphRulesComponent (NodeP node,unsigned *ancest);
NodeP RemoveAliasNodeIdInDetermineComponent (NodeP node);

#ifdef CLEAN2
void ClearOpenDefinitionModules (void);
void AddOpenDefinitionModule (SymbolP moduleNameSymbol, DefMod definitionModule);
#endif
