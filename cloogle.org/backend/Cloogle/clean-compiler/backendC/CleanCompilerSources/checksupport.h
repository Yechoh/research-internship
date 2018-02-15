
#define cTypeDelimiter	';'	/* also in optimisations.c */
#define cTypeFirstArg	'<'
#define cTypeLastArg	'>'

extern char *ConvertSymbolKindToString (SymbKind skind);

extern void CheckError (char *msg1,char *msg2);
extern void CheckSymbolError (struct symbol *symbol,char *msg);
extern void CheckWarning (char *msg1,char *msg2);
extern void CheckSymbolWarning (struct symbol *symbol,char *msg);
extern void CheckWarningOrError (Bool error,char *msg1,char *msg2);
extern void CheckWarningOrError2 (Bool error,char *msg1,char *msg2,char *msg3);
extern void CheckSymbolWarningOrError (Bool error,struct symbol *symbol,char *msg);
extern void TupleError (void);

extern char *Earity,*Enodeid3,*Ecyclicsyn,*Enodeid2,*EwrongdefS,*Einfix_imp_def,
	*EImplandDef1,*EImplandDef5;

extern unsigned RuleCount,TypeSymbolCount;
extern SymbDef StackTop;

#define PushOnDepStack(sdef) \
	sdef->sdef_parent=StackTop; \
	StackTop=sdef

#define PopFromDepStack(sdef) \
	sdef=StackTop; \
	StackTop=sdef->sdef_parent; \
	sdef->sdef_parent=NULL

#define IsOnDepStack(sdef) ((sdef)->sdef_parent!=NULL)

#define NameOfSymbol(symb)	((symb)->symb_def ->sdef_ident->ident_name)

extern void PrintSymbolOfIdent (Ident sid,unsigned line_nr,File file);

