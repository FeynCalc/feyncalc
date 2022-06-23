(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopFindTopologyMappings*)


(* ::Text:: *)
(*`FCLoopFindTopologyMappings[{topo1, topo2, ...}]` finds mappings between topologies (written as `FCTopology` objects) `topo1, topo2, ...`. For each source topology the function returns a list of loop momentum shifts and a `GLI` replacement rule needed to map it to the given target topology. If you need to map everything to a particular set of target topologies, you can specify them via the `PreferredTopologies` option.*)


(* ::Text:: *)
(*The output is a list of two lists, the former containing the mappings and  the latter enumerating the final contributing topologies*)


(* ::Text:: *)
(*To enable shifts in the external momenta you need to set the option `Momentum` to `All`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopFindTopologies](FCLoopFindTopologies.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Here we have a set of 5 topologies*)


topos1={
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
{p1,p2,p3},{Q},{},{}]};


(* ::Text:: *)
(*3 of them can be mapped to the other two*)


mappings1=FCLoopFindTopologyMappings[topos1];


mappings1[[1]]


(* ::Text:: *)
(*And these are the final topologies*)


mappings1[[2]]


(* ::Text:: *)
(*Here is another example*)


topos2={FCTopology[fctopology1,{SFAD[{{q2,0},{0,1},1}],
SFAD[{{q1,0},{0,1},1}],SFAD[{{q1+q2,0},{0,1},1}],SFAD[{{p+q1,0},{0,1},1}],
SFAD[{{p-q2,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[fctopology2,{SFAD[{{q2,0},{0,1},1}],SFAD[{{q1,0},{0,1},1}],
SFAD[{{p+q2,0},{0,1},1}],SFAD[{{p-q1,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[fctopology3,{SFAD[{{q2,0},{0,1},1}],SFAD[{{q1,0},{0,1},1}],
SFAD[{{p-q1,0},{0,1},1}],SFAD[{{p-q1+q2,0},{0,1},1}]},{q1,q2},{p},{},{}]}


(* ::Text:: *)
(*Yet this time we have some preferred set of topologies and want to match to them (if possible)*)


preferredTopos2={FCTopology[prop2L,{SFAD[{{q1,0},{0,1},1}],
SFAD[{{q2,0},{0,1},1}],SFAD[{{q1-q2,0},{0,1},1}],SFAD[{{-p+q1,0},{0,1},1}],
SFAD[{{-p+q2,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[prop2LX1,{SFAD[{{q2,0},{0,1},1}],SFAD[{{q1-q2,0},{0,1},1}],
SFAD[{{-p+q1,0},{0,1},1}],SFAD[{{-p+q2,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[prop2LX3,{SFAD[{{q1,0},{0,1},1}],SFAD[{{q2,0},{0,1},1}],
SFAD[{{-p+q1,0},{0,1},1}],SFAD[{{-p+q2,0},{0,1},1}]},{q1,q2},{p},{},{}],
FCTopology[prop2LX15,{SFAD[{{q2,0},{0,1},1}],SFAD[{{q1-q2,0},{0,1},1}],
SFAD[{{-p+q1,0},{0,1},1}]},{q1,q2},{p},{},{}]}


mappings2=FCLoopFindTopologyMappings[topos2,PreferredTopologies->preferredTopos2];


mappings2[[1]]


(* ::Text:: *)
(*And these are the final occurring topologies*)


mappings2[[2]]


(* ::Text:: *)
(*If we need to match subtopologies into larger topologies, we first need to generate all possible subtopologies for each relevant topology.*)


topos3={
FCTopology[fctopology1, {
SFAD[{{l1 + l2 - q1, 0}, {0, 1}, 1}], 
SFAD[{{l2, 0}, {SMP["m_t"]^2, 1}, 1}], 
SFAD[{{l1, 0}, {SMP["m_t"]^2, 1}, 1}], 
SFAD[{{l2 + q2, 0}, {SMP["m_t"]^2, 1}, 1}], 
SFAD[{{l1 - q1, 0}, {SMP["m_t"]^2, 1}, 1}], 
SFAD[{{l1 - q1 - q2, 0}, {SMP["m_t"]^2, 1}, 1}]}, {l1, l2}, {q1, q2}, {}, {}], 
FCTopology[fctopology9, {
SFAD[{{l1 + l2 + q2, 0}, {0, 1}, 1}], 
SFAD[{{l2, 0}, {SMP["m_t"]^2, 1}, 1}], 
SFAD[{{l1, 0}, {SMP["m_t"]^2, 1}, 1}], 
SFAD[{{l1 + q2, 0}, {SMP["m_t"]^2, 1}, 1}], 
SFAD[{{l1 - q1, 0}, {SMP["m_t"]^2, 1}, 1}]}, {l1, l2}, {q1, q2}, {}, {}] 
 }


subTopos3=Flatten[FCLoopFindSubtopologies[topos3]];


subTopos3//Length


(* ::Text:: *)
(*Now we can match a smaller topology into a larger topology*)


mappings3=FCLoopFindTopologyMappings[topos3,PreferredTopologies->subTopos3];


mappings3[[1]]


mappings3[[2]]


(* ::Text:: *)
(*Mapping the following two topologies onto each other requires shifts in the external momenta due to the chosen kinematic constraints.*)


topos4={
FCTopology[topo1, {
SFAD[{{l1 + q1, 0}, {m^2, 1}, 1}], 
SFAD[{{l1 - l2, 0}, {0, 1}, 1}], 
SFAD[{{l2 + q1, 0}, {m^2, 1}, 1}], 
SFAD[{{l2 - q2, 0}, {m^2, 1}, 1}], 
SFAD[{{l2, 0}, {0, 1}, 1}]}, {l1, l2}, {q1, q2}, {SPD[q1, q1] -> 0, SPD[q2, q2] -> 0, SPD[q1, q2] -> s/2}, {}],
FCTopology[topo2, {
SFAD[{{l1 - l2, 0}, {m^2, 1}, 1}], 
SFAD[{{l1 - q2, 0}, {0, 1}, 1}], 
SFAD[{{l2 - q2, 0}, {m^2, 1}, 1}], 
SFAD[{{l2 + q1, 0}, {m^2, 1}, 1}], 
SFAD[{{l2, 0}, {0, 1}, 1}]}, {l1, l2}, {q1, q2}, {SPD[q1, q1] -> 0, SPD[q2, q2] -> 0, SPD[q1, q2] -> s/2}, {}]}


mappings4=FCLoopFindTopologyMappings[topos4,Momentum->All];


mappings4[[1]]


(* ::Text:: *)
(*Otherwise no mappings exist*)


FCLoopFindTopologyMappings[topos4][[1]]
