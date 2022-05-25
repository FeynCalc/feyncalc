(* ::Package:: *)

 


(* ::Section:: *)
(*FCTopology*)


(* ::Text:: *)
(*`FCTopology[id, {prop1, prop2, ...}, {l1, l2, ...}, {p1, p2, ...}, {kRule1, kRule2, ...}, {}]` denotes a topology with the identifier `id` that is characterized by the propagators `{prop1, prop2, ...}`. The propagators in the list do not necessarily have to form a valid basis, i.e. the basis may also be incomplete or overdetermined. The lists `{l1, l2, ...}` and `{p1, p2, ...}` stand for the loop and external momenta respectively. Furthermore, {kRule1, kRule2, ...} denotes replacement rules for kinematic invariants.*)


(* ::Text:: *)
(*The last argument (an empty list) is reserved for future improvements.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopValidTopologyQ](FCLoopValidTopologyQ.md), [GLI](GLI.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A 2-loop topology with one external momentum `Q`*)


FCTopology[topo1,{SFAD[p1],SFAD[p2],SFAD[Q-p1-p2],SFAD[Q-p2],SFAD[Q-p1]},{p1,p2},{Q},{},{}]


(* ::Text:: *)
(*A 3-loop topology with one external momentum `Q`*)


topo=FCTopology[topo2,{SFAD[p1],SFAD[p2],SFAD[p3],SFAD[Q-p1-p2-p3],SFAD[Q-p1-p2],
SFAD[Q-p1],SFAD[Q-p2],SFAD[p1+p3],SFAD[p2+p3]},{p1,p2,p3},{Q},{},{}]


(* ::Text:: *)
(*Use `FCLoopValidTopologyQ` to check if the syntax of the given topology is correct.*)


FCLoopValidTopologyQ[topo]
