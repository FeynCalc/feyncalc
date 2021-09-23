(* ::Package:: *)

 


(* ::Section:: *)
(*QuarkGluonVertex*)


(* ::Text:: *)
(*`QuarkGluonVertex[\[Mu], a]` gives the Feynman rule for the quark-gluon vertex.*)


(* ::Text:: *)
(*`QGV` can be used as an abbreviation of `QuarkGluonVertex`.*)


(* ::Text:: *)
(*The dimension and the name of the coupling constant are determined by the options `Dimension` and `CouplingConstant`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GluonVertex](GluonVertex.md).*)


(* ::Subsection:: *)
(*Examples*)


QuarkGluonVertex[\[Mu],a,Explicit->True]


QGV[\[Mu],a]


Explicit[%]


QuarkGluonVertex[\[Mu],a,CounterTerm ->1,Explicit->True]


QuarkGluonVertex[\[Mu],a,CounterTerm ->2,Explicit->True]


QuarkGluonVertex[\[Mu],a,CounterTerm ->3,Explicit->True]


QuarkGluonVertex[{p,\[Mu],a},{q},{k},OPE->True,Explicit->True]


QuarkGluonVertex[{p,\[Mu],a},{q},{k},OPE->False,Explicit->True]
