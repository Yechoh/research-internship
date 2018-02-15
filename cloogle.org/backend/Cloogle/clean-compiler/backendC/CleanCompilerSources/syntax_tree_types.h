/*
	Version 1.2 17 dec1996
*/

#define BASIC_TYPE_IDS_STRING "ibcrfswpvr" /* indexed by SymbKind */

#define Type_Variable_Mark	(1 << Nr_Of_Basic_Types)

typedef enum
{	NoUniAttr, NotUniqueAttr, UniqueAttr, ExistsAttr, UniqueVariable, FirstUniVarNumber
} UniquenessAttributeKind;

typedef unsigned AttributeKind;

typedef struct poly_list
{	void *				pl_elem;
	struct poly_list *	pl_next;
} * PolyList;

typedef struct type_arg * TypeArgs, TypeArg;
typedef struct type_node *	TypeNode;
typedef struct type_alt *	TypeAlts;

typedef struct
{	BITVECT	tac_uniprop;
	BITVECT	tac_possign;
	BITVECT	tac_negsign;		
} TypeArgClass;

typedef struct type_var *TypeVar;

typedef struct type_var_list
{
	TypeVar					tvl_elem;
	struct type_var_list *	tvl_next;
	AttributeKind			tvl_attribute;
} * TypeVarList;

typedef struct flat_type 
{
	Symbol					ft_symbol;
	TypeVarList				ft_arguments;
	AttributeKind			ft_attribute;
} * FlatType;

STRUCT (symbol_list, SymbolList)
{
	struct symbol_def *		sl_symbol;
	struct symbol_list *	sl_next;
};

STRUCT (type_context, TypeContext)
{
	SymbolList				tyco_symbols;
	TypeVar					tyco_variable;
	struct type_context	*	tyco_next;
};

typedef struct field_list
{
	Symbol				fl_symbol;
	TypeNode			fl_type;
	StateS				fl_state;
	struct field_list *	fl_next;
} * FieldList;

typedef struct constructor_list
{
	TypeNode					cl_constructor;
	FieldList					cl_fields;
	StateP						cl_state_p; /* for constructors, union met cl_fields ? */
	struct constructor_list *	cl_next;
} * ConstructorList;

typedef struct type
{
	FlatType			type_lhs;
	ConstructorList		type_constructors;
	struct type *		type_next;
	unsigned			type_line;
	int					type_nr_of_constructors;	/* 0 for records */
} * Types;

#define type_fields 	type_constructors -> cl_fields
#define type_symbol		type_lhs -> ft_symbol

struct rule_type
{	TypeAlts			rule_type_rule;
	StateP              rule_type_state_p;
	TypeNode			rule_type_root;
	unsigned			rule_type_line;
};

typedef struct syn_type SynType,*SynTypes;

struct syn_type
{	FlatType 			syn_lhs;
};

typedef struct abs_type
{	FlatType			abs_graph;
	struct abs_type *	abs_next;
	unsigned			abs_line;
} *AbsTypes;

struct type_node
{
	union
	{	TypeVar				contents_tv;
		Symbol				contents_symbol;
	} type_node_contents;

	struct type_arg *		type_node_arguments;
	AttributeKind			type_node_attribute;
	short					type_node_arity;
	Annotation				type_node_annotation;
	unsigned char			type_node_is_var:1;
	TypeVarList				type_for_all_vars;
};

#define type_node_symbol type_node_contents.contents_symbol
#define type_node_tv type_node_contents.contents_tv

struct type_arg
{	TypeNode	type_arg_node;
	TypeArgs	type_arg_next;
};

typedef struct attr_kind_list
{	AttributeKind			akl_elem;
	struct attr_kind_list *	akl_next;
} * AttributeKindList;
	
typedef struct uni_var_equats
{	AttributeKind			uve_demanded;
	AttributeKindList		uve_offered;
	struct uni_var_equats *	uve_next;
} * UniVarEquations;

STRUCT (strict_positions, StrictPositions)
{
	int sp_size;		/* size in bits */
	int sp_bits [1];	/* variable size */
};

typedef struct type_alt
{
	TypeNode				type_alt_lhs;
	TypeNode				type_alt_rhs;
	UniVarEquations			type_alt_attr_equations;
	TypeContext				type_alt_type_context;
	unsigned				type_alt_line;
	StrictPositionsP		type_alt_strict_positions;
} TypeAlt;

struct type_var
{
	Ident			tv_ident;
	unsigned short	tv_mark;
	int				tv_refcount;
	int 			tv_argument_nr;
	int				tv_overvar_arity;
	TypeVar			tv_imp_tv;
	struct type_cell *	tv_type;
};

#define TestMark(n,f,mask) 	(((n)->f & (mask)) != 0)

#define TV_EXISTENTIAL_ATTRIBUTE_MASK				(1 << 3)	/* checktypedefs, typeconv */
#define TV_WITH_INST_RESTR							(1 << 14)	/* checktypedefs */

typedef struct uni_var
{
	Ident				uv_ident;
	unsigned short		uv_mark;
	int					uv_number;
	struct uni_var *	uv_next_uni_var;
	UniVarEquations		uv_equations;
} * UniVar;

#ifdef SHORT_CLASS_NAMES
STRUCT (module_info, ModuleInfo)
{
	struct type_conversion_table *	mi_type_table;
};

STRUCT (type_conversion_table, TypeConversionTable) 
{	int								tct_number;
	struct symbol_def *				tct_type_symbol;
	struct type_conversion_table *	tct_next;
};
#endif
