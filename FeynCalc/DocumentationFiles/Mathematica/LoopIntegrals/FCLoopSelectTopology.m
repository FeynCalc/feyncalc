(* ::Package:: *)

(* ::Section:: *)
(*FCLoopSelectTopology*)


(* ::Text:: *)
(*`FCLoopSelectTopology[int, topos]` selects the topology that matches the `GLI` `int` from a list of topologies `topos`.*)


(* ::Text:: *)
(*The first argument can be also a list, in which case the function will return a list of matching topologies.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopSelectTopology[{GLI[topo2,{2,2}]},{\
FCTopology[topo2,{FAD[{p1,m}],FAD[{p1-q,m}]},{p1},{q},{},{}],\
FCTopology[topo1,{FAD[{p1,m}],FAD[{p1-q,m}]},{p1},{q},{SPD[q]->M^2,\
SPD[q2]->M2^2},{}]}]
