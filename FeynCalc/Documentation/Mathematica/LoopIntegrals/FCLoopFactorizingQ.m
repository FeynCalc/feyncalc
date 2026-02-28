(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFactorizingQ*)


(* ::Text:: *)
(*`FCLoopFactorizinQI[int, topo]` checks whether the given loop integral factorizes or not. The input can be made integrals in the `GLI` or `FAD` notation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopFactorizingSplit](FCLoopFactorizingSplit.md), [FCLoopCreateFactorizingRules](FCLoopCreateFactorizingRules.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopFactorizingQ[FAD[{k,m}],{k}]


FCLoopFactorizingQ[FAD[{k1,m1},{k2,m2},{k1-k2}],{k1,k2}]


FCLoopFactorizingQ[FAD[{k1,m1},{k2,m2}],{k1,k2}]


int=FAD[{k1,m1},{k1-p1},{k2,m2},{k2-p2}]/.k1->k1+k2


FCLoopFactorizingQ[int,{k1,k2}]
