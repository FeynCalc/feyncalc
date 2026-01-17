(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFindScalelessTopologies*)


(* ::Text:: *)
(*`FCLoopFindScalelessTopologies[topos]` finds scaleless topologies with in the given list of topologies. The function returns a list of two lists, where the first list contains all scaleless topologies and the second one the rest.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindOverdeterminedTopologies](FCLoopFindOverdeterminedTopologies.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),*)
(*[SubtopologyMarker](SubtopologyMarker.md).*)


(* ::Subsection:: *)
(*Examples*)


topos={FCTopology[topo1,{SFAD[k1],SFAD[k1+p],SFAD[k1-p]},{k1},{p},{},{}],
FCTopology[topo2,{SFAD[k1],SFAD[k1+p]},{k1},{p},{},{}],
FCTopology[topo3,{SFAD[k1]},{k1},{p},{},{}]}


FCLoopFindScalelessTopologies[topos]
