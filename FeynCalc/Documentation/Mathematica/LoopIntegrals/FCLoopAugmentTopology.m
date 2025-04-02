(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopAugmentTopology*)


(* ::Text:: *)
(*`FCLoopAugmentTopology[topo, {extraProps}]` augments the topology `topo` by adding new propagators `extraProps` to the basis. This is usually needed when a tensor reduction requires us to introduce an auxiliary vector that will appear in scalar products involving loop momenta.*)


(* ::Text:: *)
(*The input topologies do not have to be complete.*)


(* ::Text:: *)
(*The output of this routine contains augmented topologies and a list of replacement rules for converting `GLI`s depending on the old topologies into new ones.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopTensorReduce](FCLoopTensorReduce.md).*)


(* ::Subsection:: *)
(*Examples*)


topo=FCTopology["topo1",{SFAD[{q1,m^2}],SFAD[{q1+p1}],
	SFAD[{q1+p2}]},{q1},{p1,p2},{Hold[SPD][p1]->0,Hold[SPD][p2]->0,
	Hold[SPD][p1,p2]->0},{}]


(* ::Text:: *)
(*The option `AugmentedTopologyMarker` denotes a symbol that is usually introduced by `FCLoopTensorReduce` when the reduction requires an auxiliary vector. Therefore, it will appear on the right hand side of the `GLI`-replacement rules. This can be disabled by setting this option to `False`*)


FCLoopAugmentTopology[topo,{SFAD[{{0,q1.n}}]},
FinalSubstitutions->{Hold[SPD][n]->0,Hold[SPD][n,p1]->np1,
Hold[SPD][n,p2]->np2}]


FCLoopAugmentTopology[topo,{SFAD[{{0,q1.n}}]},
FinalSubstitutions->{Hold[SPD][n]->0,Hold[SPD][n,p1]->np1,
Hold[SPD][n,p2]->np2},AugmentedTopologyMarker->False]
