(* ::Package:: *)

 


(* ::Section:: *)
(*GHP*)


(* ::Text:: *)
(*`GHP[p, a, b]` gives the ghost propagator where `a` and `b` are the color indices.*)


(* ::Text:: *)
(*`GHP[p]` omits the $\delta _{ab}$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GhostPropagator](GhostPropagator.md), [GluonPropagator](GluonPropagator.md), [GluonGhostVertex](GluonGhostVertex.md).*)


(* ::Subsection:: *)
(*Examples*)


GHP[p,a,b]


GHP[p]//Explicit


GHP[p,c1,c2]


StandardForm[FCE[GHP[-k,c3,c4]//Explicit]]
