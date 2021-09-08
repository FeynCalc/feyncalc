(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFindTopologyMappings*)


(* ::Text:: *)
(*`FCLoopFindTopologyMappings[{topo1, topo2, ...}]` finds mappings between topologies (written as `FCTopology` objects) `topo1, topo2, ...`. For each source topology the function returns a list of loop momentum shifts and a `GLI` replacement rule needed to map it to the given target topology. If you need to map everything to a particular set of target topologies, you can specify them via the `PreferredTopologies` option.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopFindTopologies](FCLoopFindTopologies.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopFindTopologyMappings[{
FCTopology[fctopology1,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],
SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],
SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology2,{SFAD[{{p3,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],
SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p2-Q,0},{0,1},1}],
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology3,{SFAD[{{p3,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p1+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],
SFAD[{{p1+p3-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},
{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology4,{SFAD[{{p3,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p1+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],
SFAD[{{p1+p3-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},
{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology5,{SFAD[{{p3,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p1+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],
SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},
{p1,p2,p3},{Q},{},{}]}]


FCLoopFindTopologyMappings[{FCTopology[fctopology1,{SFAD[{{q2,0},{0,1},1}],
SFAD[{{q1,0},{0,1},1}],SFAD[{{q1+q2,0},{0,1},1}],SFAD[{{p+q1,0},{0,1},1}],
SFAD[{{p-q2,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[fctopology2,{SFAD[{{q2,0},{0,1},1}],SFAD[{{q1,0},{0,1},1}],
SFAD[{{p+q2,0},{0,1},1}],SFAD[{{p-q1,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[fctopology3,{SFAD[{{q2,0},{0,1},1}],SFAD[{{q1,0},{0,1},1}],
SFAD[{{p-q1,0},{0,1},1}],SFAD[{{p-q1+q2,0},{0,1},1}]},{q1,q2},{p},{},{}]},
PreferredTopologies->{FCTopology[prop2L,{SFAD[{{q1,0},{0,1},1}],
SFAD[{{q2,0},{0,1},1}],SFAD[{{q1-q2,0},{0,1},1}],SFAD[{{-p+q1,0},{0,1},1}],
SFAD[{{-p+q2,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[prop2LX1,{SFAD[{{q2,0},{0,1},1}],SFAD[{{q1-q2,0},{0,1},1}],
SFAD[{{-p+q1,0},{0,1},1}],SFAD[{{-p+q2,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[prop2LX3,{SFAD[{{q1,0},{0,1},1}],SFAD[{{q2,0},{0,1},1}],
SFAD[{{-p+q1,0},{0,1},1}],SFAD[{{-p+q2,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[prop2LX15,{SFAD[{{q2,0},{0,1},1}],SFAD[{{q1-q2,0},{0,1},1}],
SFAD[{{-p+q1,0},{0,1},1}]},{q1,q2},{p},{},{}]}]
