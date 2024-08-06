(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFindMomentumShifts*)


(* ::Text:: *)
(*`FCLoopFindMomentumShifts[source, target, {p1, p2, ...}]` finds loop momentum shifts that bring loop integrals or topologies in the list `source` to the form specified in target. The integrals/topologies in `intFrom` and `intTo` are assumed to be equivalent and their denominators must be properly ordered via `FCLoopToPakForm`. Here the loop momenta `p1, p2, ...` belong to the source topologies.*)


(* ::Text:: *)
(*`target` must be provided as a list of `FeynAmpDenominator` objects, while `intFrom` is a list of such lists.*)


(* ::Text:: *)
(*It is also possible to invoke the function as `FCLoopFindMomentumShifts[{FCTopology[...], FCTopology[...]}, FCTopology[...]]`.*)


(* ::Text:: *)
(*For topologies involving kinematic constraints some mappings may require shifts not only in the loop but also in the external momenta. Such shifts are disabled by default but can be activated by setting the option `Momentum` to `All`. This option can be dangerous, because the amplitude does not necessarily have to be symmetric under shifts of external momenta!*)


(* ::Text:: *)
(*Normally, `FCLoopFindMomentumShifts` will abort the evaluation if it fails to find any suitable shifts. Setting the option `Abort` to `False` will force the function to merely return an empty list in such situations.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopPakOrder](FCLoopPakOrder.md).*)


(* ::Subsection:: *)
(*Examples*)


source={{FAD[p4], FAD[p1], FAD[p1 - p3 - p4],
FAD[{p1 - p4, m1}], FAD[{p3, m1}], FAD[p3 + q1],
FAD[p1 + q1]}}


target={FAD[p4], FAD[p1 + p4 + q1], FAD[p1 - p3 + q1],
FAD[{p1 + q1, m1}], FAD[{p3, m1}], FAD[p3 + q1],
FAD[p1 + p4 + 2 q1]}


FCLoopFindMomentumShifts[source, target, {p1, p3, p4}]


FCLoopFindMomentumShifts[{{FAD[r4],FAD[r1],FAD[r1-p3-r4],
FAD[{r1-r4,m1}],FAD[{p3,m1}],FAD[p3+q1],FAD[r1+q1]}},
{FAD[p4],FAD[p1+p4+q1],FAD[p1-p3+q1],FAD[{p1+q1,m1}],
FAD[{p3,m1}],FAD[p3+q1],FAD[p1+p4+2 q1]},{p1,p3,p4,r4,r1}]


source1={FCTopology[
fctopology3, {SFAD[{{p1, 0}, {0, 1}, 1}],
	SFAD[{{p3, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p2 + p3, 0}, {0, 1}, 1}],
	SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p2, 0}, {0, 1}, 1}]},{p1,p2,p3},{Q},{},{}],
FCTopology[
fctopology4, {SFAD[{{p2 + p3, 0}, {0, 1}, 1}],
	SFAD[{{p3, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p3, 0}, {0, 1}, 1}]},{p1,p2,p3},{Q},{},{}]}


target1=FCTopology[
fctopology1, {SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}],
SFAD[{{p3, 0}, {0, 1}, 1}],
SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}],
SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}],
SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}],
SFAD[{{p2 + p3, 0}, {0, 1}, 1}],
SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}]},{p1,p2,p3},{Q},{},{}]


FCLoopFindMomentumShifts[source1,target1]


source2={FCTopology[topo1, {
SFAD[{{l1 + q1, 0}, {m^2, 1}, 1}], 
SFAD[{{l1 - l2, 0}, {0, 1}, 1}], 
SFAD[{{l2 + q1, 0}, {m^2, 1}, 1}], 
SFAD[{{l2 - q2, 0}, {m^2, 1}, 1}], 
SFAD[{{l2, 0}, {0, 1}, 1}]}, {l1, l2}, {q1, q2}, 
{SPD[q1, q1] -> 0, SPD[q2, q2] -> 0, SPD[q1, q2] -> s/2}, {}]}


target2=FCTopology[topo2, {
SFAD[{{l1 - l2, 0}, {m^2, 1}, 1}], 
SFAD[{{l1 - q2, 0}, {0, 1}, 1}], 
SFAD[{{l2 - q2, 0}, {m^2, 1}, 1}], 
SFAD[{{l2 + q1, 0}, {m^2, 1}, 1}], 
SFAD[{{l2, 0}, {0, 1}, 1}]}, {l1, l2}, {q1, q2}, 
{SPD[q1, q1] -> 0, SPD[q2, q2] -> 0, SPD[q1, q2] -> s/2}, {}]


(* ::Text:: *)
(*Mapping these two topologies onto each other requires shifts in the external momenta*)


Quiet[FCLoopFindMomentumShifts[source2,target2,Abort->False]]


(* ::Text:: *)
(*Once we allow such shifts, everything works as expected*)


FCLoopFindMomentumShifts[source2,target2,Momentum->All]


(* ::Text:: *)
(*For equivalent topologies containing mixed quadratic-eikonal propagators it's often not possible to find suitable shifts because the function cannot reconstruct the correct momentum flow through such propagators*)


source3={FCTopology["pfrTopo303",{SFAD[{{k2,-2 gkin meta k2 . n+meta u0b k2 . nb},
{2 gkin meta^2 u0b,1},1}],SFAD[{{k1-k2,meta u0b (-k1+k2) . nb},{0,1},1}],
SFAD[{{k1,-2 gkin meta k1 . n},{0,1},1}],SFAD[{{k1,-meta u0b k1 . nb},{0,1},1}],
SFAD[{{0,-k2 . nb},{0,1},1}],SFAD[{{0,-k1 . nb},{0,1},1}]},{k1,k2},{n,nb},
{Hold[SPD][n]->0,Hold[SPD][nb]->0,Hold[SPD][n,nb]->2},{}]}


target3=FCTopology["pfrTopo267",{SFAD[{{k2,0},{0,1},1}],SFAD[{{k1-k2,2 gkin meta k1 . n-2 gkin meta u0b k1 . n-2 gkin meta k2 . n+2 gkin meta u0b k2 . n+meta u0b (-k1+k2) . nb},{2 gkin meta^2 u0b-2 gkin meta^2 u0b^2,1},1}],SFAD[{{k1,2 gkin meta k1 . n-2 gkin meta u0b k1 . n-meta u0b k1 . nb},{2 gkin meta^2 u0b-2 gkin meta^2 u0b^2,1},1}],SFAD[{{k1,-2 gkin meta u0b k1 . n},{0,1},1}],SFAD[{{0,k2 . nb},{2 gkin meta,1},1}],SFAD[{{0,k1 . nb},{2 gkin meta u0b,1},1}]},{k1,k2},{n,nb},{Hold[SPD][n]->0,Hold[SPD][nb]->0,Hold[SPD][n,nb]->2},{}]


(DataType[#,FCVariable]=True)&/@\!\(TraditionalForm\`{gkin, meta, u0b}\);


FCLoopFindMomentumShifts[source3,target3]


(* ::Text:: *)
(*To this aim one can try converting those mixed propagators to purely quadratic ones using `FCLoopReplaceQuadraticEikonalPropagators`*)


source3New=FCLoopReplaceQuadraticEikonalPropagators[source3,LoopMomenta->{k1,k2},
InitialSubstitutions->{ExpandScalarProduct[SPD[k1-k2]]->SPD[k1-k2]},
IntermediateSubstitutions->{SPD[n]->0,SPD[nb]->0,SPD[n,nb]->2}]


target3New=FCLoopReplaceQuadraticEikonalPropagators[target3,LoopMomenta->{k1,k2},
InitialSubstitutions->{ExpandScalarProduct[SPD[k1-k2]]->SPD[k1-k2]},
IntermediateSubstitutions->{SPD[n]->0,SPD[nb]->0,SPD[n,nb]->2}]//First


(* ::Text:: *)
(*With the new topologies everything works as expected*)


FCLoopFindMomentumShifts[source3New,target3New]
