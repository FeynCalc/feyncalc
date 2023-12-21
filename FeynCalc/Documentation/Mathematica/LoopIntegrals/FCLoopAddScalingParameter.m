(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopAddScalingParameter*)


(* ::Text:: *)
(*`FCLoopAddScalingParameter[topo, la, rules]` multiplies masses and momenta in the propagators of the topology `topo` by the scaling parameter `la` according to the scaling rules in `rules`. The `id` of the topology remains unchanged. This is useful e.g. for asymptotic expansions of the corresponding loop integrals*)
(*given as GLIs.*)


(* ::Text:: *)
(*The scaling variable should be declared as `FCVariable` via the `DataType` mechanism.*)


(* ::Text:: *)
(*Notice that if all terms in a propagator have the same scaling, the scaling variable in the respective propagator will be set to unity.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).*)


(* ::Subsection:: *)
(*Examples*)


DataType[la,FCVariable]=True;


(* ::Text:: *)
(*We declare the external 4-momentum `q` as our hard scale, while the mass `mc` is soft*)


topoScaled=FCLoopAddScalingParameter[FCTopology[prop1LtopoC11,{SFAD[{{I p1,0},{-mc^2,-1},1}],
SFAD[{{I (p1-q),0},{-mc^2,-1},1}]},{p1},{q},{SPD[q,q]->mb^2},{}],la,
{q->la^0 q,mc->la^1 mc}]


(* ::Text:: *)
(*Having set up the scaling we can now use `FCLoopGLIExpand` to expand the loop integrals belonging to this topology up to*)
(*the desired order in `la`. Here we choose $\mathcal{O}(\lambda^4)$*)


FCLoopGLIExpand[GLI[prop1LtopoC11,{1,1}],{topoScaled},{la,0,4}]
