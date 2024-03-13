(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopGLIRaiseDimension*)


(* ::Text:: *)
(*`FCLoopGLIRaiseDimension[gli, topo]` raises the dimension of the given `GLI` from N to N+2 and expresses it in terms of `N`-dimensional loop integrals returned in the output.*)


(* ::Text:: *)
(*The algorithm is based on the code of  the function `RaisingDRR` from R. Lee's LiteRed*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopGLILowerDimension](FCLoopGLILowerDimension.md).*)


(* ::Subsection:: *)
(*Examples*)


topo=FCTopology[
topo1, {SFAD[p1], SFAD[p2], SFAD[Q - p1 - p2], SFAD[Q - p2],
SFAD[Q - p1]}, {p1, p2}, {Q}, {Hold[SPD[Q]] -> qq}, {}]


FCLoopGLIRaiseDimension[GLI[topo1, {1, 1, 1, 1, 1}],topo]


FCLoopGLIRaiseDimension[GLI[topo1, {n1,n2, n3, 1, 1}],topo]
