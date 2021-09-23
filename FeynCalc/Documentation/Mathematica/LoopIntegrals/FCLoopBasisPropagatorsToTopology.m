(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopBasisPropagatorsToTopology*)


(* ::Text:: *)
(*`FCLoopBasisPropagatorsToTopology[{prop1, prop2, ...}]` takes the list of Pairs and FeynAmpDenominators and converts it into a list of propagators that can be used to describe a topology.*)


(* ::Text:: *)
(*The input can also consist of an `FCTopology` object or a list thereof.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopBasisIntegralToPropagators](FCLoopBasisIntegralToPropagators.md).*)


(* ::Subsection:: *)
(*Examples*)


{FAD[q]}
FCLoopBasisPropagatorsToTopology[%]


{FAD[{q,m}]}
FCLoopBasisPropagatorsToTopology[%]


{FAD[{q,m}],SPD[q,p]}
FCLoopBasisPropagatorsToTopology[%]


FCLoopBasisPropagatorsToTopology[{FCTopology[topo1, {SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p3, 0}, {mb^2, 1}, 1}], SFAD[{{p1 + p3, 0}, {mb^2, 1}, 1}], SFAD[{{p1 - q, 0}, {mb^2, 1}, 1}], SFAD[{{0, p3 . q}, {0, 1}, 1}]}, 
 {p1, p3}, {q}, {}, {}],FCTopology[topo1, {SFAD[{{p1, 0}, {mb^2, 1}, 1}], SFAD[{{p3, 0}, {mb^2, 1}, 1}], 
 SFAD[{{p1 + p3, 0}, {mb^2, 1}, 1}], SFAD[{{p1 - q, 0}, {mb^2, 1}, 1}], SFAD[{{0, (p3+p1) . q}, {0, 1}, 1}]}, 
 {p1, p3}, {q}, {}, {}]}]
