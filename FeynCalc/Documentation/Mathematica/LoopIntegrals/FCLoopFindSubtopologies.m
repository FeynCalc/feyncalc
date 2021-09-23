(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFindSubtopologies*)


(* ::Text:: *)
(*`FCLoopFindSubtopologies[topo]` finds all nonvanishing subtopologies of the FCTopology `topo`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopFindSubtopologies[FCTopology[TRI,{SFAD[{{p1,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1+Q1,0},{0,1},1}],SFAD[{{p1+p2+Q1,0},
{0,1},1}],SFAD[{{-p1+Q2,0},{0,1},1}],SFAD[{{-p1-p2+Q2,0},{0,1},1}]},
{p1,p2},{Q1,Q2},{},{}]]


FCLoopFindSubtopologies[FCTopology[topo1,{SFAD[{{p3, 0}, {0, 1}, 1}], 
SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}],
SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, 
{0, 1}, 1}]},{p1,p2,p3},{Q},{},{}],FCE->True]
