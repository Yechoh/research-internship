definition module predef

import syntax, hashtable

::	PredefinedSymbols	:== {# PredefinedSymbol}

::	PredefinedSymbol = {
		pds_module	:: !Index,
		pds_def		:: !Index
	}

init_identifiers :: !*SymbolTable !*World -> (!*SymbolTable,!*World)

predefined_idents :: {!Ident}

buildPredefinedSymbols :: !*HashTable -> (!.PredefinedSymbols,!*HashTable)

buildPredefinedModule :: !Bool !*PredefinedSymbols -> (!ScannedModule, !.PredefinedSymbols)

cPredefinedModuleIndex :== 1

PD_StringTypeIndex :== 0
PD_Arity2TupleTypeIndex :== 8
PD_Arity32TupleTypeIndex :== 38

/* identifiers not present the hashtable */

PD_PredefinedModule			:== 0

FirstTypePredefinedSymbolIndex:==PD_StringType; // to compute index in com_type_defs

PD_StringType				:== 1

PD_ListType :== 2
PD_StrictListType :== 3
PD_UnboxedListType :== 4
PD_TailStrictListType :== 5
PD_StrictTailStrictListType :== 6
PD_UnboxedTailStrictListType :== 7
PD_OverloadedListType :== 8

PD_Arity2TupleType			:== 9
PD_Arity32TupleType			:== 39

PD_LazyArrayType			:== 40
PD_StrictArrayType			:== 41
PD_UnboxedArrayType			:== 42

PD_UnitType :== 43

// constructors:

FirstConstructorPredefinedSymbolIndex :== PD_ConsSymbol; // to compute index in com_cons_defs

PD_ConsSymbol :== 44
PD_StrictConsSymbol :== 45
PD_UnboxedConsSymbol :== 46
PD_TailStrictConsSymbol :== 47
PD_StrictTailStrictConsSymbol :== 48
PD_UnboxedTailStrictConsSymbol :== 49
PD_OverloadedConsSymbol :== 50

PD_NilSymbol :== 51
PD_StrictNilSymbol :== 52
PD_UnboxedNilSymbol :== 53
PD_TailStrictNilSymbol :== 54
PD_StrictTailStrictNilSymbol :== 55
PD_UnboxedTailStrictNilSymbol :== 56
PD_OverloadedNilSymbol :== 57

PD_Arity2TupleSymbol		:== 58
PD_Arity32TupleSymbol		:== 88

PD_UnitConsSymbol :== 89

// end constructors

PD_TypeVar_a0				:== 90
PD_TypeVar_a31				:== 121

/* identifiers present in the hashtable */

PD_StdArray					:== 122
PD_StdEnum					:== 123
PD_StdBool					:== 124

PD_AndOp					:== 125
PD_OrOp						:== 126

/* Array functions */

PD_ArrayClass				:== 127

PD_CreateArrayFun			:== 128
PD__CreateArrayFun			:== 129
PD_ArraySelectFun			:== 130
PD_UnqArraySelectFun		:== 131
PD_ArrayUpdateFun			:== 132
PD_ArrayReplaceFun			:== 133
PD_ArraySizeFun				:== 134
PD_UnqArraySizeFun			:== 135

/* Enum/Comprehension functions */

PD_SmallerFun				:== 136
PD_LessOrEqualFun			:== 137
PD_IncFun					:== 138
PD_SubFun					:== 139
PD_From						:== 140
PD_FromThen					:== 141
PD_FromTo					:== 142
PD_FromThenTo				:== 143

/* StdMisc */
PD_StdMisc					:== 144
PD_abort					:== 145
PD_undef					:== 146

PD_Start					:== 147

PD_DummyForStrictAliasFun	:== 148

PD_StdStrictLists:==149

PD_cons:==150
PD_decons:==151

PD_cons_u:==152
PD_decons_u:==153

PD_cons_uts:==154
PD_decons_uts:==155

PD_nil:==156
PD_nil_u:==157
PD_nil_uts:==158

PD_ListClass :== 159
PD_UListClass :== 160
PD_UTSListClass :== 161

/* Dynamics */

// TC class
PD_TypeCodeMember			:== 162
PD_TypeCodeClass			:== 163
// dynamic module
PD_StdDynamic				:== 164
// dynamic type
PD_Dyn_DynamicTemp				:== 165
// type code (type)
PD_Dyn_TypeCode					:== 166
// unification (type)
PD_Dyn_UnificationEnvironment	:== 167
// type code (expressions)
PD_Dyn_TypeScheme			:== 168
PD_Dyn_TypeApp				:== 169
PD_Dyn_TypeVar				:== 170
PD_Dyn_TypeCons				:== 171
PD_Dyn_TypeUnique			:== 172
PD_Dyn__TypeFixedVar		:== 173
// unification (expressions)
PD_Dyn_initial_unification_environment	:== 174
PD_Dyn_bind_global_type_pattern_var		:== 175
PD_Dyn_unify							:== 176
PD_Dyn_normalise						:== 177

/* Generics */
PD_StdGeneric				:== 178
// Generics types
PD_TypeBimap				:== 179
PD_TypeUNIT					:== 180
PD_TypeEITHER				:== 181
PD_TypePAIR					:== 182
// for constructor info
PD_TypeCONS					:== 183
PD_TypeRECORD				:== 184
PD_TypeFIELD				:== 185
PD_TypeOBJECT				:== 186
PD_TGenericConsDescriptor	:== 187
PD_TGenericRecordDescriptor	:== 188
PD_TGenericFieldDescriptor 	:== 189
PD_TGenericTypeDefDescriptor :== 190
PD_TGenConsPrio				:== 191
PD_TGenConsAssoc			:== 192
PD_TGenType					:== 193

PD_TypeGenericDict 			:== 194
PD_TypeGenericDict0			:== 195
// Generics fields
PD_map_to					:== 196
PD_map_from					:== 197
// Generics expression
PD_ConsBimap				:== 198
PD_ConsUNIT					:== 199
PD_ConsLEFT					:== 200
PD_ConsRIGHT				:== 201
PD_ConsPAIR					:== 202
// for constructor info
PD_ConsCONS					:== 203
PD_ConsRECORD				:== 204
PD_ConsFIELD				:== 205
PD_ConsOBJECT				:== 206
PD_CGenericConsDescriptor 	:== 207
PD_CGenericRecordDescriptor	:== 208
PD_CGenericFieldDescriptor 	:== 209
PD_CGenericTypeDefDescriptor :== 210
PD_CGenConsNoPrio			:== 211
PD_CGenConsPrio				:== 212
PD_CGenConsAssocNone		:== 213
PD_CGenConsAssocLeft		:== 214
PD_CGenConsAssocRight		:== 215
PD_CGenTypeCons				:== 216
PD_CGenTypeVar				:== 217
PD_CGenTypeArrow			:== 218
PD_CGenTypeApp				:== 219

PD_bimapId					:== 220
PD_GenericBimap				:== 221

PD_FromS					:== 222
PD_FromTS					:== 223
PD_FromSTS					:== 224
PD_FromU					:== 225
PD_FromUTS					:== 226
PD_FromO					:== 227

PD_FromThenS				:== 228
PD_FromThenTS				:== 229
PD_FromThenSTS				:== 230
PD_FromThenU				:== 231
PD_FromThenUTS				:== 232
PD_FromThenO				:== 233

PD_FromToS					:== 234
PD_FromToTS					:== 235
PD_FromToSTS				:== 236
PD_FromToU					:== 237
PD_FromToUTS				:== 238
PD_FromToO					:== 239

PD_FromThenToS				:== 240
PD_FromThenToTS				:== 241
PD_FromThenToSTS			:== 242
PD_FromThenToU				:== 243
PD_FromThenToUTS			:== 244
PD_FromThenToO				:== 245

PD_Dyn__to_TypeCodeConstructor	:== 246
PD_TypeCodeConstructor :== 247

PD_TC_Int			:== 248
PD_TC_Char			:== 249
PD_TC_Real			:== 250
PD_TC_Bool			:== 251
PD_TC_Dynamic		:== 252
PD_TC_File			:== 253
PD_TC_World			:== 254

PD_TC__Arrow		:== 255

PD_TC__List			:== 256
PD_TC__StrictList	:== 257
PD_TC__UnboxedList	:== 258
PD_TC__TailStrictList	:== 259
PD_TC__StrictTailStrictList	:== 260
PD_TC__UnboxedTailStrictList	:== 261

PD_TC__Tuple2		:== 262
PD_TC__Tuple3		:== 263
PD_TC__Tuple4		:== 264
PD_TC__Tuple5		:== 265
PD_TC__Tuple6		:== 266
PD_TC__Tuple7		:== 267
PD_TC__Tuple8		:== 268
PD_TC__Tuple9		:== 269
PD_TC__Tuple10		:== 270
PD_TC__Tuple11		:== 271
PD_TC__Tuple12		:== 272
PD_TC__Tuple13		:== 273
PD_TC__Tuple14		:== 274
PD_TC__Tuple15		:== 275
PD_TC__Tuple16		:== 276
PD_TC__Tuple17		:== 277
PD_TC__Tuple18		:== 278
PD_TC__Tuple19		:== 279
PD_TC__Tuple20		:== 280
PD_TC__Tuple21		:== 281
PD_TC__Tuple22		:== 282
PD_TC__Tuple23		:== 283
PD_TC__Tuple24		:== 284
PD_TC__Tuple25		:== 285
PD_TC__Tuple26		:== 286
PD_TC__Tuple27		:== 287
PD_TC__Tuple28		:== 288
PD_TC__Tuple29		:== 289
PD_TC__Tuple30		:== 290
PD_TC__Tuple31		:== 291
PD_TC__Tuple32		:== 292

PD_TC__LazyArray	:== 293
PD_TC__StrictArray	:== 294
PD_TC__UnboxedArray	:== 295

PD_TC__Unit			:== 296

PD_NrOfPredefSymbols		:== 297

GetTupleConsIndex tup_arity :== PD_Arity2TupleSymbol + tup_arity - 2
GetTupleTypeIndex tup_arity :== PD_Arity2TupleType + tup_arity - 2

// changes requires recompile of {static,dynamic}-linker plus all dynamics ever made
UnderscoreSystemDynamicModule_String	:== "_SystemDynamic"	

// List-type
PD_ListType_String				:== "_List"
PD_ConsSymbol_String			:== "_Cons"
PD_NilSymbol_String				:== "_Nil"

// Array-type
PD_UnboxedArray_String			:== "_#Array"

DynamicRepresentation_String			:== "DynamicTemp" // "_DynamicTemp"		
