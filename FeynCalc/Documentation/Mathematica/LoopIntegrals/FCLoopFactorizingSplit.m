(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFactorizingSplit*)


(* ::Text:: *)
(*`FCLoopFactorizingSplit[int, topo]` checks whether the given loop integral factorizes and separates it into factorizing integrals if it is the case. The input can be made integrals in the `GLI` or `FAD` notation.*)


(* ::Text:: *)
(*Notice that the output is always given in the `FAD` notation even if the input was provided using `GLI`s.*)


(* ::Text:: *)
(*In most cases the routine will fail to find a factorization of integrals with numerators.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopFactorizingQ](FCLoopFactorizingQ.md), [FCLoopCreateFactorizingRules](FCLoopCreateFactorizingRules.md).*)


(* ::Subsection:: *)
(*Examples*)


FCReloadFunctionFromFile[FCLoopFactorizingSplit]


FCLoopFactorizingSplit[{FAD[{k,m}]},{k}]


FCLoopFactorizingSplit[FAD[{k1,m1},{k2,m2},{k1-k2}],{k1,k2}]


FCLoopFactorizingSplit[FAD[{k1,m1},{k2,m2}],{k1,k2}]


int=FAD[{k1,m1},{k1-p1},{k2,m2},{k2-p2}]/.k1->k1+k2


FCLoopFactorizingSplit[int,{k1,k2}]
