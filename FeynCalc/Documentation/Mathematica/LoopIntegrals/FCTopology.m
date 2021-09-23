(* ::Package:: *)

 


(* ::Section:: *)
(*FCTopology*)


(* ::Text:: *)
(*`FCTopology[id, {prop1, prop2, ...}]` denotes a topology with the identifier id that is characterized by the propagators `{prop1, prop2, ...}`. The propagators in the list do not necessarily have to form a valid basis, i.e. the basis may also be incomplete or overdetermined.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A 2-loop topology with one external momentum `Q`*)


FCTopology[topo1,{SFAD[p1],SFAD[p2],SFAD[Q-p1-p2],SFAD[Q-p2],SFAD[Q-p1]}]


(* ::Text:: *)
(*A 3-loop topology with one external momentum `Q`*)


FCTopology[topo2,{SFAD[p1],SFAD[p2],SFAD[p3],SFAD[Q-p1-p2-p3],SFAD[Q-p1-p2],SFAD[Q-p1],SFAD[Q-p2],SFAD[p1+p3],SFAD[p2+p3]}]
