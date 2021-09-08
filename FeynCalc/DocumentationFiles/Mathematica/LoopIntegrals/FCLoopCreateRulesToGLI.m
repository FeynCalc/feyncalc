(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopCreateRulesToGLI*)


(* ::Text:: *)
(*`FCLoopCreateRulesToGLI[topo]` creates replacement rules for converting numerators from the given topology to GLI objects with inverse propagators.*)


(* ::Text:: *)
(*It is also possible to use `FCLoopCreateRulesToGLI[{topo1, topo2, ...}]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCFindTopologies](FCFindTopologies.md), [FCFindTopologyMappings](FCFindTopologyMappings.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*1-loop tadpole*)


FCLoopCreateRulesToGLI[FCTopology[topo1,{SFAD[{p1,m^2}]},{p1},{},{},{}]]


(* ::Text:: *)
(*2-loop tadpole with 3 different masses*)


FCLoopCreateRulesToGLI[FCTopology[topo1,{SFAD[{p1,m1^2}],SFAD[{p2,m2^2}],SFAD[{p1-p2,m3^2}]},{p1,p2},{},{},{}]]


(* ::Text:: *)
(*2-loop self-energy*)


FCLoopCreateRulesToGLI[FCTopology["prop2Lv1",
 {SFAD[{p1, m1^2}], SFAD[{p2, m2^2}], SFAD[p1 - q], SFAD[p2 - q], SFAD[{p1 - p2, m3^2}]}, {p1, p2}, {Q}, {}, {}]]


(* ::Text:: *)
(*A list of 3-loop self-energy topologies*)


topoList={
FCTopology["prop3Lv1",{SFAD[p1],SFAD[p2],SFAD[p3],SFAD[p1-p2],SFAD[p2-p3],SFAD[p1+q1],
	SFAD[p2+q1],SFAD[p3+q1],SFAD[{{0,p1 . p3}}]},{p1,p2,p3},{q1},{},{}],

FCTopology["prop3L2",{SFAD[p1],SFAD[p2],SFAD[p3],SFAD[p1-p2],SFAD[p2-p3],SFAD[p1-p3],
	SFAD[p1+q1],SFAD[p3+q1],SFAD[{{0,(p1-p2) . q1}}]},{p1,p2,p3},{q1},{},{}],
	
FCTopology["prop3L3",{SFAD[p1],SFAD[p1-p4],SFAD[p3],SFAD[p4],SFAD[p1-p3-p4],SFAD[p1+q1],
	SFAD[p3+p4+q1],SFAD[p3+q1],SFAD[{{0,(p4) . q1}}]},{p1,p3,p4},{q1},{},{}]	
}


FCLoopCreateRulesToGLI[topoList]
