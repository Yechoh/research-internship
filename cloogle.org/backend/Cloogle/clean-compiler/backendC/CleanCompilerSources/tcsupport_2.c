/*
	Version 1.2 21/01/1997

	Author:  Sjaak Smetsers
*/

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "settings.h"
#include "syntaxtr.t"
#include "comsupport.h"

#include "sizes.h"
#include "checker.h"
#include "checksupport.h"
#include "typeconv.h"
#include "tcsupport.h"
#include "scanner.h"
#include "comparser.h"
#include "buildtree.h"

void PrintNodeSymbol (Node node, int arg_nr, File file)
{
	Symbol rootsymb;
	
	switch (node -> node_kind)
	{
	case IfNode:
		switch (arg_nr)
		{
		case 1:	FPutS ("condition part of guard or if rule", file);
				return;
		case 2:	FPutS ("then part of guard or if rule", file);
				return;
		case 3:	FPutS ("else part of guard or if rule", file);
				return;
		default:	FPutS ("guard or if rule", file);
				return;
		}
		break;
	case SelectorNode:
		if (arg_nr == 1)
			FPutS ("argument of selection", file);
		else
			FPutS ("selection", file);
		return;
	case MatchNode:
		if (arg_nr == 1)
		{	FPutS ("rhs selection of", file);
			break;
		}
		else
		{	FPutS ("rhs selection", file);
			return;
		}
	case UpdateNode:
		FPutS ("update of record", file);
		break;
	case NodeIdNode:
		if (node -> node_node_id -> nid_ident != NULL)
		{	Ident id =  node -> node_node_id -> nid_ident;
			if (TestMark (node -> node_node_id, nid_mark2, NID_FIELD_NAME_MASK))
			{	SymbDef rec_symb = (SymbDef) id -> ident_environ;
			
				FPrintF (file, "field %s of record %s", id -> ident_name, rec_symb -> sdef_ident -> ident_name);
			}
			else
				FPutS (id  -> ident_name, file);
		}
		else if (node -> node_node_id -> nid_node)
			PrintNodeSymbol (node -> node_node_id -> nid_node, 0, file);
		return;
	default:
		break;
	}

	rootsymb = node -> node_symbol;
	
	if (rootsymb -> symb_kind == select_symb)
	{	if (arg_nr == 1)
		{	FPrintF (file, "%d-tuple selection of ", rootsymb -> symb_arity);
			PrintNodeSymbol (node -> node_arguments -> arg_node, 0, file);
		}
		else
			FPrintF (file, "selection of the %d-th argument of a %d-tuple ", node -> node_arity, rootsymb -> symb_arity);
	}
	else if (rootsymb -> symb_kind == apply_symb)				
	{	if (arg_nr == 1)
			PrintNodeSymbol (node -> node_arguments -> arg_node, 0, file);
		else
		{	Node argnode;
			for (arg_nr = 1, argnode = node -> node_arguments -> arg_node; 
				argnode -> node_kind == NormalNode && argnode -> node_symbol -> symb_kind == apply_symb;
				argnode = argnode -> node_arguments -> arg_node)
				arg_nr ++;
			PrintNodeSymbol (argnode, arg_nr, file);
		}
	}
	else if (rootsymb -> symb_kind == tuple_symb)
	{	int tup_arity = node -> node_arity;
		FPutS ("(_", file);
		for (tup_arity--; tup_arity > 0; tup_arity--)
			FPutS (",_", file);
		FPutC (')', file);
	}
	else
	{	if (arg_nr > 0)
			FPrintF (StdError, "argument %d of ", arg_nr);
		PrintSymbol (rootsymb, file);
	}

} /* PrintNodeSymbol */
