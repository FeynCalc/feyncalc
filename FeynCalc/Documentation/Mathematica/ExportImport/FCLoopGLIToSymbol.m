(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopGLIToSymbol*)


(* ::Text:: *)
(*`FCLoopGLIToSymbol[exp]` converts `GLI`s to symbols.*)


(* ::Text:: *)
(*The option `Head` determines the prefix of the symbol and can be set to `FCTopology` (default) or `GLI`*)


(* ::Text:: *)
(*The option `Character` specifies the separator between to prefix and the indices.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GLI](GLI.md), [SMPToSymbol](SMPToSymbol.md), [FCGVToSymbol](FCGVToSymbol.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopGLIToSymbol[GLI[topo1,{1,1,1,1,1}]]


FCLoopGLIToSymbol[GLI[topo1,{1,1,1,1,1}],Head->GLI]


FCLoopGLIToSymbol[GLI[topo1,{1,1,1,1,1}],Character->"$"]
