(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopFindTopologyMappings*)


(* ::Text:: *)
(*`FCLoopFindTopologyMappings[{topo1, topo2, ...}]` finds mappings between topologies (written as `FCTopology` objects) `topo1, topo2, ...`. For each source topology the function returns a list of loop momentum shifts and a `GLI` replacement rule needed to map it to the given target topology.*)


(* ::Text:: *)
(*The mappings are being identified using Pak's algorithm. Once a group of identical topologies has been found, the algorithm will try to map all of them to the first topology in the list. All topologies that have been successfully mapped to the first topology are then removed from the list (including the target topology) and the same procedure is repeated for the remaining topologies until there are no topologies left in the group. *)


(* ::Text:: *)
(*Notice that not every Pak mapping between topologies can be converted to a mapping in terms of loop momentum shifts. Some of the identified mappings only exist on the level of loop integrals but not topologies.*)


(* ::Text:: *)
(*The output is a list of two lists, the former containing the mappings and  the latter enumerating the final contributing topologies*)


(* ::Text:: *)
(*To enable exchanges of external momenta (e.g. $p_i \leftrightarrow p_j$) you need to set the option `Momentum` to `All`. Notice that this usually makes sense only for a very specific set of processes (e.g. QCD diagrams with massless partons). Exchanging the momenta of say a massive and a massless particle will obviously lead to inconsistent results.*)


(* ::Text:: *)
(*If you need to map everything to a particular set of target topologies, you can specify them via the `PreferredTopologies` option.  The usage of this option may have some side effects that one should be aware of.*)


(* ::Text:: *)
(*- If a topology `topo1` appears in the input but not in the preferred topologies list, it can be mapped to one of the preferred topologies or otherwise to some other input topologies. This usually happens when preferred topologies and input topologies are completely distinct.*)


(* ::Text:: *)
(*- If a topology `topo1` appears only in the preferred topologies list, then some other topologies from the input can be mapped to it. However, any mappings between `topo1` and other preferred topologies will be automatically discarded. This behavior is intentional  and helps to keep the code logic simple and straightforward. Therefore, the list of preferred topologies is tacitly expected to contain only unique topologies. Supplying a list with topologies that can be mapped to each other will not cause errors but it may result in mappings that include more topologies than necessary.*)


(* ::Text:: *)
(*- If a topology `topo1` appears both in the input and in the preferred topologies list, then it will be regarded as a preferred topology only. This means that only some other topologies from the input can be mapped to it. However, `topo1` will not be mapped to other preferred topologies, even though such mappings may exist. This is why it is better to avoid situations where the same topologies appear in both lists.*)


(* ::Text:: *)
(*In real life the output of `FCLoopFindSubtopologies` is often used as the value for the `PreferredTopologies` option with the aim of finding mappings between smaller and larger topologies. In this case one has to distinguish between the following situations*)


(* ::Text:: *)
(*- `FCLoopFindSubtopologies` is applied to the same list of topologies that is passed as input to `FCLoopFindTopologyMappings`. Here `FCLoopFindSubtopologies` removes the original input topologies from its output by default. Hence, there are no topologies appearing both in the input and preferred topologies lists.*)


(* ::Text:: *)
(*- `FCLoopFindSubtopologies` is applied to a list of preferred topologies that are distinct from the input topologies. In this case one should set the option `Remove` to `False` to ensure that the original preferred topologies are kept in the output.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopFindSubtopologies](FCLoopFindSubtopologies.md), [FCLoopFindTopologies](FCLoopFindTopologies.md).*)


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


(* ::Text:: *)
(*Of course, one has  to check that the suggested shift of external momenta is consistent! By default, amplitudes are not guaranteed to*)
(*remain invariant under such shifts.*)


mappings4[[1]]


(* ::Text:: *)
(*Otherwise no mappings exist*)


FCLoopFindTopologyMappings[topos4][[1]]


(* ::Text:: *)
(*Topologies containing eikonal or other nonstandard propagators may introduce additional challenges.*)
(*Even though two such topologies can be recognized to be identical, the code still would not be able to*)
(*work out the correct momentum shifts without some additional input.*)


topoEik1=FCTopology[mytopo67,{SFAD[{{k2,0},{0,1},1}],SFAD[{{k1,0},{0,1},1}],
SFAD[{{k1+k2,0},{0,1},1}],SFAD[{{0,-k1 . nb},{0,1},1}],
SFAD[{{k2,-meta u0b k2 . nb},{0,1},1}],SFAD[{{k1+k2,-2 gkin meta u0b (k1+k2) . n},
{0,1},1}],SFAD[{{k1,-2 gkin meta k1 . n+meta u0b k1 . nb},{2 gkin meta^2 u0b,1},1}]},
{k1,k2},{n,nb},{Hold[SPD][n]->0,Hold[SPD][nb]->0,Hold[SPD][n,nb]->2},{}];


topoEik2=FCTopology[mytopo79,{SFAD[{{k2,0},{0,1},1}],SFAD[{{k1,0},{0,1},1}],
SFAD[{{0,k1 . nb},{0,1},1}],SFAD[{{k2,-meta u0b k2 . nb},{0,1},1}],
SFAD[{{k1+k2,-meta u0b (k1+k2) . nb},{0,1},1}],SFAD[{{k1,
2 gkin meta k1 . n-meta u0b k1 . nb},{2 gkin meta^2 u0b,1},1}],
SFAD[{{k1+k2,2 gkin meta u0b (k1+k2) . n-meta u0b (k1+k2) . nb},
{2 gkin meta^2 u0b^2,1},1}]},{k1,k2},{n,nb},{Hold[SPD][n]->0,
Hold[SPD][nb]->0,Hold[SPD][n,nb]->2},{}];


DataType[gkin,FCVariable]=True;
DataType[meta,FCVariable]=True;
DataType[u0b,FCVariable]=True;


(* ::Text:: *)
(*At first sight these two topologies are independent from each other*)


FCLoopFindTopologyMappings[{topoEik1,topoEik2}];


(* ::Text:: *)
(*However, if we tell the code how some eikonal propagators can be brought into a quadratic form,*)
(*then an explicit mapping can be found*)


toposNew=FCLoopReplaceQuadraticEikonalPropagators[{topoEik1,topoEik2},
LoopMomenta->{k1,k2},
InitialSubstitutions->{
ExpandScalarProduct[SPD[k1-k2]]->SPD[k1-k2],
ExpandScalarProduct[SPD[k1+k2]]->SPD[k1+k2]},
IntermediateSubstitutions->{SPD[n]->0,SPD[nb]->0,SPD[n,nb]->2}];


eikMappings=FCLoopFindTopologyMappings[toposNew];

