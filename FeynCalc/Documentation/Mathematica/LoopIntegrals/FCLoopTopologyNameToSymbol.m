(* ::Package:: *)

(* ::Section:: *)
(*FCLoopTopologyNameToSymbol*)


(* ::Text:: *)
(*`FCLoopTopologyNameToSymbol[exp]` converts topology names in `FCTopology`s and `GLI`s that are strings to expressions. This can be useful when exporting expressions generated with Mathematica to other software tools.*)


(* ::Text:: *)
(*Using the option `Except` one can exclude certain names from the conversion process.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopTopologyNameToSymbol[GLI["a1",{1,1,1}]]//InputForm


FCLoopTopologyNameToSymbol[FCTopology["topo2",{FAD[{p1,m}],FAD[{p1-q,m}]},{p1},{q},{},{}]]//InputForm


FCLoopTopologyNameToSymbol[GLI["a1",{1,1,1}]+GLI["b1",{1,1,1}],Except->{"a"}]//InputForm
