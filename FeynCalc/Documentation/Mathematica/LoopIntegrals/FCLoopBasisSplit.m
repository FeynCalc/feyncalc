(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopBasisSplit*)


(* ::Text:: *)
(*`FCLoopBasisSplit[int, {q1, q2, ...}]` checks if the given loop integral factorizes and if so splits it into independent integrals.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


FCI@FAD[{q1,m},{q2,m},{p1-p2,0}]
FCLoopBasisSplit[%,{q1,q2},Head->loopInt]


FCI[SFAD[q1,q1-q2,q2,{q3,m^2}]]
FCLoopBasisSplit[%,{q1,q2,q3},Head->loop,FCE->True]
