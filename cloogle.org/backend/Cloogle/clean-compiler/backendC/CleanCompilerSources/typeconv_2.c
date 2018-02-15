/*
Version 1.0 26/08/1994
Author: Sjaak Smetsers 
*/

#define STATES_GENERATED
#define STORE_UNIQUE_ATTRIBUTES_IN_TYPE_NODES

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "settings.h"
#include "syntaxtr.t"
#include "comsupport.h"

#include "scanner.h"
#include "comparser.h"
#include "sizes.h"
#include "checker.h"
#include "transform.h"
#include "sa.h"
#include "tcsupport.h"
#include "typeconv.h"
#include "checksupport.h"
#include "statesgen.h"
#include "buildtree.h"

typedef
	struct attr_ref_count_info
	{	BITVECT			  		arci_used;
		BITVECT			  		arci_multiply_used;
		BITVECT			  		arci_implicitly_attributed;
		struct attr_ref_count_info  * arci_next;
	} * ARC_Info;

static ARC_Info CurrentARC_Info;

static char *TypeConv = "typeconv";

static unsigned RetrieveRefCountInfo (int attr_var, Bool *used_implicitly)
{
	ARC_Info attrinfo = CurrentARC_Info;
	unsigned newnumber = 0;
	int i;
	
	while (attr_var >= 32)
	{	attr_var -= 32;
		for (i = 0; i < 32; i++)
		{	if (BITTEST (attrinfo -> arci_multiply_used, i))
				newnumber++;
		}
		attrinfo = attrinfo -> arci_next;
		Assume (attrinfo != NULL, TypeConv, "RetrieveRefCountInfo");
	}
	if (BITTEST (attrinfo -> arci_multiply_used, attr_var))
	{	for (i = 0; i < attr_var; i++)
		{	if (BITTEST (attrinfo -> arci_multiply_used, i))
				newnumber++;
		}
		*used_implicitly = False;
		return newnumber + 1;
	}
	else
	{	*used_implicitly = BITTEST (attrinfo -> arci_implicitly_attributed, attr_var);
		return 0;
	}
		
} /* RetrieveRefCountInfo */

static char *PrintVars = "abcdefghijklmnopqrst";
#define NrOfPrintVars 20

static char *PrintUniVars = "uvwxyz";
#define NrOfPrintUniVars 6

#define cDoPrintAnnot	True
#define cDontPrintAnnot	False

static void PrintNode (TypeNode node, Bool brackets, Bool strict_context, Bool print_annot);
static unsigned RetrieveRefCountInfo (int attr_var, Bool *used_implicitly);

static void PrintAttributeVariable (unsigned attr_nr)
{
	if (attr_nr <= NrOfPrintUniVars)
		FPrintF (StdListTypes, "%c", PrintUniVars [attr_nr - 1]);
	else
		FPrintF (StdListTypes, "u%d", attr_nr - NrOfPrintUniVars);

} /* PrintAttributeVariable */

extern Bool DoShowAttributes;

#define cDoPrintColon	True

static Bool PrintAttribute (AttributeKind attr, Bool print_colon)
{
	if (attr == UniqueAttr)
	{	FPutC ('*', StdListTypes);
		return True;
	}
	else if (DoShowAttributes)
	{	Bool used_implicitly;
		unsigned attr_nr = RetrieveRefCountInfo (attr - FirstUniVarNumber, & used_implicitly);

		if (attr_nr == 0)
		{	if (! used_implicitly)
			{	FPutC ('.', StdListTypes);
				return True;
			}
			else
				return False;
		}
		else
		{	PrintAttributeVariable (attr_nr);
			if (print_colon)
				FPutC (':', StdListTypes);
			return True;
		}
	}
	else
		return False;
	
} /* PrintAttribute */

#define cDoPrintAttribute	True
#define cDontPrintAttribute	False

#define cInAStrictContext	True
#define cNotInAStrictContext	False

#define cPrintBrackets		True
#define cDontPrintBrackets	False


static void PrintArgument (TypeArgs arg, Bool brackets, Bool strict_context, Bool print_attribute)
{
	if (arg -> type_arg_node -> type_node_is_var)
	{	if (strict_context)
#ifdef STATES_GENERATED
# if 1
			strict_context = arg -> type_arg_node -> type_node_annotation==StrictAnnot;
# else
			strict_context = !IsLazyState (arg -> type_arg_node -> type_node_state);
# endif
#else
			strict_context = arg -> type_arg_node -> type_node_state.state_kind == StrictOnA;
#endif
	
		if (	strict_context && (DoListAllTypes || DoListStrictTypes) &&
#ifdef STATES_GENERATED
# if 1
			arg -> type_arg_node -> type_node_annotation==StrictAnnot)
# else
			!IsLazyState (arg -> type_arg_node -> type_node_state))
# endif
#else
			arg -> type_arg_node -> type_node_state.state_kind == StrictOnA)
#endif
			FPutC ('!', StdListTypes);

		if (print_attribute && arg -> type_arg_node -> type_node_attribute > NoAttr)
			PrintAttribute (arg -> type_arg_node -> type_node_attribute, arg -> type_arg_node -> type_node_tv != NULL);

		if (arg -> type_arg_node -> type_node_tv)
		{	if (arg -> type_arg_node -> type_node_tv -> tv_ident)
				FPutS (arg -> type_arg_node -> type_node_tv -> tv_ident -> ident_name, StdListTypes);
			else
				FPrintF (StdListTypes, "i%ld", arg -> type_arg_node -> type_node_tv);
		}
	}
	else
		PrintNode (arg -> type_arg_node, brackets, strict_context, cDoPrintAnnot);

} /* PrintArgument */

static void PrintArguments (TypeArgs args, char separator, Bool brackets, Bool strict_context, FlatType form_type)
{
	if (args)
	{	int arg_nr;
		TypeVarList form_type_vars;
		
		if (form_type != NULL)
		{	form_type_vars = form_type -> ft_arguments;

			PrintArgument (args, brackets, strict_context, ! TestMark (form_type_vars -> tvl_elem, tv_mark, TV_EXISTENTIAL_ATTRIBUTE_MASK));
			form_type_vars = form_type_vars -> tvl_next;
		}
		else
		{	form_type_vars = NULL;
			PrintArgument (args, brackets, strict_context, cDoPrintAttribute);
		}

		for (arg_nr = 1, args = args -> type_arg_next; args; args = args -> type_arg_next, arg_nr++)
		{	FPutC (separator, StdListTypes);
				
			if (form_type_vars != NULL)
			{	PrintArgument (args, brackets, strict_context, ! TestMark (form_type_vars -> tvl_elem, tv_mark, TV_EXISTENTIAL_ATTRIBUTE_MASK));
				form_type_vars = form_type_vars -> tvl_next;
			}
			else
				PrintArgument (args, brackets, strict_context, cDoPrintAttribute);
		}
	}
	
} /* PrintArguments */

#ifdef CLEAN2
static void PrintTypeVarList (TypeVarList type_vars)
{
	for (; type_vars != NULL; type_vars = type_vars -> tvl_next)
	{
		/* RWS:
			Printing the attributes currently works because the attributes for
			universally quantified type variables can only be none, '*' or '.'.
			For attribute variables something should probably done with the
			CurrentARC_Info administration, but I don't understand how this works. 
		*/
		if (type_vars -> tvl_attribute != NoUniAttr)
			PrintAttribute (type_vars -> tvl_attribute, cDoPrintColon);

		FPutS (type_vars -> tvl_elem -> tv_ident -> ident_name, StdListTypes);

		if (type_vars -> tvl_next != NULL)
			FPutC (' ', StdListTypes);		
	}
}
#endif

static FlatType RetrieveLhsOfTypeDefinition (SymbDef tdef)
{
	switch (tdef -> sdef_kind)
	{
	case TYPE:
	case RECORDTYPE:
		return tdef -> sdef_type != NULL ? tdef -> sdef_type -> type_lhs : NULL;
	case TYPESYN:
		return tdef -> sdef_syn_type -> syn_lhs;
		break;
	case ABSTYPE:
		return tdef -> sdef_abs_type -> abs_graph;
		break;
	default:
		return NULL;
	}
} /* RetrieveLhsOfTypeDefinition */

static void PrintNode (TypeNode node, Bool brackets, Bool strict_context, Bool print_annot)
{

	if (print_annot && strict_context && (DoListAllTypes || DoListStrictTypes) &&
#ifdef STATES_GENERATED
# if 1
		node -> type_node_annotation==StrictAnnot)
# else
		!IsLazyState (node -> type_node_state))
# endif
#else
		node -> type_node_state.state_kind == StrictOnA)
#endif
		FPutC ('!', StdListTypes);

	if (node -> type_node_attribute > NoAttr)
	{	if (PrintAttribute (node -> type_node_attribute, cDoPrintColon) &&
			(node -> type_node_symbol -> symb_kind == fun_type || node -> type_node_symbol -> symb_kind == apply_symb))
			brackets = True;
	}
#ifdef CLEAN2
	if (node -> type_for_all_vars != NULL)
	{	FPutS ("(A.", StdListTypes);
		PrintTypeVarList (node -> type_for_all_vars);
		FPutC (':', StdListTypes);
		brackets = False;
	}
#endif
	switch (node -> type_node_symbol -> symb_kind)
	{
	case tuple_type:
	{	int form_arity = node -> type_node_symbol -> symb_arity;
	
		if (node -> type_node_arity == form_arity)
		{	FPutC ('(', StdListTypes);
			PrintArguments (node -> type_node_arguments, ',', cDontPrintBrackets, strict_context, NULL);
			FPutC (')', StdListTypes);
		}
		else
		{	int i;
			if (brackets && node -> type_node_arguments)
				FPutC ('(', StdListTypes);
			FPutC ('(', StdListTypes);
			for (i=1; i<form_arity; i++)
				FPutC (',', StdListTypes);
			FPutC (')', StdListTypes);
			if (node -> type_node_arguments)
			{	PrintArguments (node -> type_node_arguments, ' ', cPrintBrackets, strict_context, NULL);
				if (brackets)
					FPutC (')', StdListTypes);
			}
		}
		break;
	}
	case list_type:
		FPutC ('[', StdListTypes);
#if STRICT_LISTS
		if (node->type_node_symbol->symb_head_strictness==2)
			FPutC ('!', StdListTypes);			
		else if (node->type_node_symbol->symb_head_strictness==3 || node->type_node_symbol->symb_head_strictness==4)
			FPutC ('#', StdListTypes);
#endif
		PrintArguments (node -> type_node_arguments, ',', cDontPrintBrackets, cNotInAStrictContext, NULL);
#if STRICT_LISTS
		if (node->type_node_symbol->symb_tail_strictness)
			FPutC ('!', StdListTypes);			
#endif
		FPutC (']', StdListTypes);
		break;
	case array_type:
		FPutS ("{", StdListTypes);
		PrintArguments (node -> type_node_arguments, ',', cDontPrintBrackets, cInAStrictContext, NULL);
		FPutS ("}", StdListTypes);
		break;
	case strict_array_type:
		FPutS ("{!", StdListTypes);
		PrintArguments (node -> type_node_arguments, ',', cDontPrintBrackets, cInAStrictContext, NULL);
		FPutS ("}", StdListTypes);
		break;
	case unboxed_array_type:
		FPutS ("{#", StdListTypes);
		PrintArguments (node -> type_node_arguments, ',', cDontPrintBrackets, cInAStrictContext, NULL);
		FPutS ("}", StdListTypes);
		break;
	case fun_type:
	{	TypeNode arg_type_node = node -> type_node_arguments -> type_arg_node;
		if (brackets)
			FPutC ('(', StdListTypes);
		PrintArgument (node -> type_node_arguments, cPrintBrackets, cNotInAStrictContext, cDoPrintAttribute);
		FPutS (" -> ", StdListTypes);
		PrintArgument (node -> type_node_arguments -> type_arg_next, cDontPrintBrackets, cNotInAStrictContext, cDoPrintAttribute);
		if (brackets)
			FPutC (')', StdListTypes);
		break;
	}
	case apply_symb:
		if (brackets)
			FPutC ('(', StdListTypes);
		PrintArguments (node -> type_node_arguments, ' ', cPrintBrackets, strict_context, NULL);
		if (brackets)
			FPutC (')', StdListTypes);
		break;
	default:
		if (brackets && node -> type_node_arguments)
			FPutC ('(', StdListTypes);
		PrintSymbol (node -> type_node_symbol, StdListTypes);
		if (node -> type_node_arguments)
		{	FlatType lhs_type;
		
			if (node -> type_node_symbol -> symb_kind == definition)
				lhs_type =  RetrieveLhsOfTypeDefinition (node -> type_node_symbol -> symb_def);
			else
				lhs_type = NULL;

			FPutC (' ', StdListTypes);
			PrintArguments (node -> type_node_arguments,' ', cPrintBrackets, strict_context, lhs_type);
			if (brackets)
				FPutC (')', StdListTypes);
		}
		break;
	}

#ifdef CLEAN2
	if (node -> type_for_all_vars != NULL)
		FPutC (')', StdListTypes);
#endif
} /* PrintNode */

static void PrintAttributeEquations (UniVarEquations attr_equas)
{
	FPutS (", [", StdListTypes);
	
	for ( ; ; )
	{	AttributeKindList next;
		Bool used_implicitly;
		unsigned dem_attr_nr = RetrieveRefCountInfo (attr_equas -> uve_demanded - FirstUniVarNumber, & used_implicitly);

		for (next = attr_equas -> uve_offered ; ; )
		{	unsigned	off_attr_nr = RetrieveRefCountInfo (next -> akl_elem - FirstUniVarNumber, & used_implicitly);

			PrintAttributeVariable (off_attr_nr);
			if ((next = next -> akl_next))
				FPutC (' ', StdListTypes);
			else
				break;
		}


		FPutS (" <= ", StdListTypes);
		PrintAttributeVariable (dem_attr_nr);

		if ((attr_equas = attr_equas -> uve_next))
			FPutS (", ", StdListTypes);
		else
			break;
	}
	FPutC (']', StdListTypes);

} /* PrintAttributeEquations */

#include <ctype.h>

void PrintTypeClass (SymbDef class_def, File file)
{
	char * class_name = class_def -> sdef_ident -> ident_name;
	
	if (*class_name == '.')
		class_name++;

	FPutS (class_name, file);

} /* PrintTypeClass */

static void PrintTypeContext (TypeContext context)
{
	SymbolList class_symbs = context -> tyco_symbols;
	TypeVar context_var = context -> tyco_variable;
	
	PrintTypeClass (class_symbs -> sl_symbol, StdListTypes);
	
	for (class_symbs = class_symbs -> sl_next; class_symbs; class_symbs = class_symbs -> sl_next)
	{	FPutS (" , ", StdListTypes);
		PrintTypeClass (class_symbs -> sl_symbol, StdListTypes);
	}

	FPutC (' ', StdListTypes);
	if (TestMark (context_var, tv_mark, TV_WITH_INST_RESTR))
		FPutC ('.', StdListTypes);
	FPutS (context_var -> tv_ident -> ident_name, StdListTypes);
	
} /* PrintTypeContext */

void PrintType (SymbDef tdef, TypeAlts type)
{
	TypeNode lhs_root = type -> type_alt_lhs;
	TypeArgs lhsargs = lhs_root -> type_node_arguments;
	
	PrintSymbolOfIdent (tdef -> sdef_ident, tdef -> sdef_line, StdListTypes);
	FPutS (" :: ", StdListTypes);
	
	if (lhsargs)
	{	PrintArguments (lhsargs,' ', cPrintBrackets, cInAStrictContext, NULL);
		FPutS (" -> ", StdListTypes);
	}
	if (type -> type_alt_rhs -> type_node_is_var)
	{	if (type -> type_alt_rhs -> type_node_attribute > NoAttr)
			PrintAttribute (type -> type_alt_rhs -> type_node_attribute, cDoPrintColon);
		FPutS (type -> type_alt_rhs -> type_node_tv -> tv_ident -> ident_name, StdListTypes);
	}
	else
	{	Bool rhs_brackets = (lhsargs == NULL) && (type -> type_alt_rhs -> type_node_symbol -> symb_kind == fun_type);
		PrintNode (type -> type_alt_rhs, rhs_brackets, cInAStrictContext, cDontPrintAnnot);
	}
	if (type -> type_alt_type_context)
	{	TypeContext next_context;
		FPutS (" | ", StdListTypes);
		PrintTypeContext (type -> type_alt_type_context);
		for (next_context = type -> type_alt_type_context -> tyco_next; next_context; next_context = next_context -> tyco_next)
		{	FPutS (" & ", StdListTypes);
			PrintTypeContext (next_context);
		}
	}
	
	if (DoShowAttributes && type -> type_alt_attr_equations)
		PrintAttributeEquations (type -> type_alt_attr_equations);

	FPutS (";\n", StdListTypes);
} /* PrintType */

void ListTypes (ImpMod imod)
{
	if (DoListAllTypes)
	{	ImpRules irule;
		for (irule = imod -> im_rules; irule; irule = irule -> rule_next)
		{	SymbDef imp_sdef = irule -> rule_root -> node_symbol -> symb_def;
		
#ifdef CLEAN2
			if (strncmp (imp_sdef->sdef_ident->ident_name, "_dictionary", 11) != 0 || imp_sdef->sdef_isused)
#endif
			PrintType (imp_sdef, irule -> rule_type);
		}
	}
} /* ListTypes */

void InitARC_Info (void)
{
	CurrentARC_Info = CompAllocType (struct attr_ref_count_info);
	CurrentARC_Info -> arci_next = NULL;

} /* InitARC_Info */
