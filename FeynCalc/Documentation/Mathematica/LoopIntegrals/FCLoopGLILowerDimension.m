(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopGLILowerDimension*)


(* ::Text:: *)
(*`FCLoopGLILowerDimension[gli, topo]` lowers the dimension of the given `GLI` from `D` to `D-2` and expresses it in terms of `D`-dimensional loop integrals returned in the output.*)


(* ::Text:: *)
(*The algorithm is based on the code of  the function `RaisingDRR` from R. Lee's LiteRed*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopGLIRaiseDimension](FCLoopGLIRaiseDimension.md).*)


(* ::Subsection:: *)
(*Examples*)


topo=FCTopology[
topo1, {SFAD[p1], SFAD[p2], SFAD[Q - p1 - p2], SFAD[Q - p2],
SFAD[Q - p1]}, {p1, p2}, {Q}, {Hold[SPD[Q]] -> qq}, {}]


FCLoopGLILowerDimension[GLI[topo1, {1, 1, 1, 1, 1}],topo]


FCLoopGLILowerDimension[GLI[topo1, {n1,n2, n3, 1, 1}],topo]
