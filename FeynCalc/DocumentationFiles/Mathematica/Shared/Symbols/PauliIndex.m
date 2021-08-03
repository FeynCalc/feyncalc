(* ::Package:: *)

 


(* ::Section:: *)
(*PauliIndex*)


(* ::Text:: *)
(*`PauliIndex` is the head of Pauli indices. The internal representation of a two-dimensional spinorial index `i` is `PauliIndex[i]`.*)


(* ::Text:: *)
(*If the first argument is an integer, `PauliIndex[i]` turns into `ExplicitPauliIndex[i]`.*)


(* ::Text:: *)
(*Pauli indices are the indices that denote the components of Pauli matrices or spinors. They should not be confused with the Cartesian indices attached to the Pauli matrices. For example in the case of $\sigma_{ij}^{k}$,  $k$ is a Lorentz index, while $i$ and $j$ are Pauli (spinorial) indices.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[PauliChain](PauliChain), [PCHN](PCHN), [ExplicitPauliIndex](ExplicitPauliIndex), [PauliIndexDelta](PauliIndexDelta), [DIDelta](DIDelta), [PauliChainJoin](PauliChainJoin), [PauliChainCombine](PauliChainCombine), [PauliChainExpand](PauliChainExpand), [PauliChainFactor](PauliChainFactor).*)


(* ::Subsection:: *)
(*Examples*)


PauliIndex[i]
%//StandardForm


PauliIndex[2]
%//StandardForm


PIDelta[i,j]//FCI//StandardForm
