(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopPropagatorsToLineMomenta*)


(* ::Text:: *)
(*`FCLoopPropagatorsToLineMomenta[{prop1, prop2, ...}]` is an auxiliary function that extracts line momenta flowing through the given list of propagators.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopIntegralToGraph](FCLoopIntegralToGraph.md), [AuxiliaryMomenta](AuxiliaryMomenta.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopPropagatorsToLineMomenta[{SFAD[{q+l,m^2}],SFAD[{p,-m^2}]},FCE->True]


FCLoopPropagatorsToLineMomenta[{CFAD[{{0,2v . (q+r)},m^2}]},FCE->True,AuxiliaryMomenta->{v}]


(* ::Text:: *)
(*Reversed signs are also supported*)


{SFAD[{I(q+l),-m^2}],SFAD[{I p,-m^2}]}
FCLoopPropagatorsToLineMomenta[%,FCE->True]


FCLoopPropagatorsToLineMomenta[{SFAD[{I(q+l),-m^2}],SFAD[{I p,-m^2}]},FCE->True]//InputForm


FCLoopPropagatorsToLineMomenta[{SFAD[{{I p1,-2 p1 . q},{0,1},1}]},FCE->True]
