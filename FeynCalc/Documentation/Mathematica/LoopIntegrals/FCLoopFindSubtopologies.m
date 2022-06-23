(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFindSubtopologies*)


(* ::Text:: *)
(*`FCLoopFindSubtopologies[topo]` finds all scalefull subtopologies of the FCTopology `topo`.*)


(* ::Text:: *)
(*Each subtopology receives a marker that specifies the topology from which it was derived. The symbol*)
(*denoting the marker is specified via the option `SubtopologyMarker`. Setting it to `False` will disable *)
(*the inclusion of the markers*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),*)
(*[SubtopologyMarker](SubtopologyMarker.md).*)


(* ::Subsection:: *)
(*Examples*)


res=FCLoopFindSubtopologies[FCTopology[TRI,{SFAD[{{p1,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1+Q1,0},{0,1},1}],SFAD[{{p1+p2+Q1,0},
{0,1},1}],SFAD[{{-p1+Q2,0},{0,1},1}],SFAD[{{-p1-p2+Q2,0},{0,1},1}]},
{p1,p2},{Q1,Q2},{},{}]];


res//Length


(* ::Text:: *)
(*Show the first three subtopologies of this 2-loop self-energy topology*)


res[[1;;3]]


res=FCLoopFindSubtopologies[FCTopology[topo1,{SFAD[{{p3, 0}, {0, 1}, 1}], 
SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}],
SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, 
{0, 1}, 1}]},{p1,p2,p3},{Q},{},{}],FCE->True];


res//Length


(* ::Text:: *)
(*Show the first three subtopologies of this 3-loop self-energy topology*)


res[[1;;3]]
