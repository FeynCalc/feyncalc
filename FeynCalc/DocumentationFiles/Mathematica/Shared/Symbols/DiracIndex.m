(* ::Package:: *)

 


(* ::Section:: *)
(*DiracIndex*)


(* ::Text:: *)
(*`DiracIndex` is the head of Dirac indices. The internal representation of a four-dimensional spinorial index `i` is `DiracIndex[i]`.*)


(* ::Text:: *)
(*If the first argument is an integer, `DiracIndex[i]` turns into `ExplicitDiracIndex[i]`.*)


(* ::Text:: *)
(*Dirac indices are the indices that denote the components of Dirac matrices or spinors. They should not be confused with the Lorentz indices attached to the Dirac matrices. For example in the case of $\gamma_{ij}^{\mu}$,  $\mu$ is a Lorentz index, while $i$ and $j$ are Dirac (spinorial) indices.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[DiracChain](DiracChain), [DCHN](DCHN), [ExplicitDiracIndex](ExplicitDiracIndex), [DiracIndexDelta](DiracIndexDelta), [DIDelta](DIDelta), [DiracChainJoin](DiracChainJoin), [DiracChainCombine](DiracChainCombine), [DiracChainExpand](DiracChainExpand), [DiracChainFactor](DiracChainFactor).*)


(* ::Subsection:: *)
(*Examples*)


DiracIndex[i]
%//StandardForm


DiracIndex[2]
%//StandardForm


DIDelta[i,j]//FCI//StandardForm
