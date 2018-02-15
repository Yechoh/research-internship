/*
	module owner: Ronny Wichers Schreur
*/
definition module backendconvert

from backend import ::BackEnd
import frontend

backEndConvertModules :: PredefinedSymbols FrontEndSyntaxTree !Int *VarHeap *AttrVarHeap *BackEnd -> (!*VarHeap, *AttrVarHeap, !*BackEnd)
