(* ::Package:: *)

 


(* ::Section:: *)
(*FCAntiSymmetrize*)


(* ::Text:: *)
(*`FCAntiSymmetrize[expr, {a1, a2, ...}]` antisymmetrizes `expr` with respect to the variables `a1, a2, ...`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCSymmetrize](FCSymmetrize).*)


(* ::Subsection:: *)
(*Examples*)


FCAntiSymmetrize[f[a,b],{a,b}]


FCAntiSymmetrize[f[x,y,z],{x,y,z}]
