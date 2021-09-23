(* ::Package:: *)

 


(* ::Section:: *)
(*PauliChainJoin*)


(* ::Text:: *)
(*`PauliChainJoin[exp]` joins chains of Pauli matrices with explicit Pauli indices wrapped with a head `PauliChain`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [DIDelta](DIDelta.md), [PauliChainCombine](PauliChainCombine.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).*)


(* ::Subsection:: *)
(*Examples*)


PCHN[PauliXi[-I],i] PCHN[CSID[a] . CSID[b],i,j] PCHN[j,PauliEta[I]]
PauliChainJoin[%]
