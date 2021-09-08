(* ::Package:: *)

 


(* ::Section:: *)
(*PCHN*)


(* ::Text:: *)
(*`PCHN[x, i, j]` is a chain of Pauli matrices `x` and is transformed into `PauliChain[FCI[x],PauliIndex[i],PauliIndex[j]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A standalone Pauli matrix with open Pauli indices*)


PCHN[CSID[a],i,j]


(* ::Text:: *)
(*A chain of Pauli matrices with open Pauli indices*)


PCHN[CSID[a] . CSID[b],i,j]


(* ::Text:: *)
(*A single $\xi ^{\dagger}$ spinor with an open Pauli index*)


PCHN[PauliXi[-I],i]


(* ::Text:: *)
(*A single $\eta ^{\dagger}$ spinor with an open Pauli index*)


PCHN[PauliEta[-I],i]


(* ::Text:: *)
(*A single $\xi$ spinor with an open Pauli index*)


PCHN[i,PauliXi[I]]


(* ::Text:: *)
(*A single $\eta$ spinor with an open Pauli index*)


PCHN[i,PauliEta[I]]


(* ::Text:: *)
(* $\xi ^{\dagger}$ spinor contracted with a chain of Pauli matrices*)


PCHN[CSID[a] . CSID[b],PauliXi[-I],j]


(* ::Text:: *)
(* $\eta ^{\dagger}$ spinor contracted with a chain of Pauli matrices*)


PCHN[CSID[a] . CSID[b],PauliEta[-I],j]


(* ::Text:: *)
(* $\xi$ spinor contracted with a chain of Pauli matrices*)


PCHN[CSID[a] . CSID[b],i,PauliXi[I]]


(* ::Text:: *)
(* $\eta$ spinor contracted with a chain of Pauli matrices*)


PCHN[CSID[a] . CSID[b],i,PauliEta[I]]
