(* ::Package:: *)

(* ::Section:: *)
(*FCSymmetrize*)


(* ::Text:: *)
(*`FCSymmetrize[expr, {a1, a2, ...}]` symmetrizes expr with respect to the variables `a1,a2, ...`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCAntiSymmetrize](FCAntiSymmetrize).*)


(* ::Subsection:: *)
(*Examples*)


FCSymmetrize[f[a,b],{a,b}]


FCSymmetrize[f[x,y,z],{x,y,z}]
