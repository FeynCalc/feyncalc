(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFindIncompleteTopologies*)


(* ::Text:: *)
(*`FCLoopFindOverdeterminedTopologies[topos]` finds topologies with incomplete propagator bases in the given list of topologies. The function returns a list of two lists, where the first list contains all incomplete topologies and the second one the rest.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindIncompleteTopologies](FCLoopRewriteIncompleteTopologies.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),*)
(*[SubtopologyMarker](SubtopologyMarker.md).*)


(* ::Subsection:: *)
(*Examples*)


topos={FCTopology[topo1,{SFAD[k1],SFAD[k1+p],SFAD[k1-p]},{k1},{p},{},{}],
FCTopology[topo2,{SFAD[k1],SFAD[k1+p]},{k1},{p},{},{}],
FCTopology[topo3,{SFAD[k1],SFAD[k2]},{k1,k2},{},{},{}]}


FCLoopFindIncompleteTopologies[topos]
