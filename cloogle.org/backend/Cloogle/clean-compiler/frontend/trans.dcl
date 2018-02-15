definition module trans

import StdEnv
import syntax,classify,predef

transformGroups :: !CleanupInfo !Int !Int !Int !Int !*{!Component} !*{#FunDef} !*{!.ConsClasses} !{# CommonDefs}  !{# {# FunType} }
		!*ImportedTypes !*TypeDefInfos !*VarHeap !*TypeHeaps !*ExpressionHeap !Bool !*File !*PredefinedSymbols
			-> (!*{!Component}, !*{#FunDef}, !*ImportedTypes, !ImportedConstructors, !*VarHeap, !*TypeHeaps, !*ExpressionHeap, !*File, !*PredefinedSymbols)
