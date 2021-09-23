(* ::Package:: *)

 


(* ::Section:: *)
(*PauliChainFactor*)


(* ::Text:: *)
(*`PauliChainFactor[exp]` factors out all expressions inside a `PauliChain` to which the chain doesn't apply. For example, all objects that are not Pauli matrices can be safely factored out from every Pauli chain.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [DIDelta](DIDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainCombine](PauliChainCombine.md), [PauliChainExpand](PauliChainExpand.md).*)


(* ::Subsection:: *)
(*Examples*)


PCHN[CV[p,\[Nu]]CSI[a] . CSI[b] . CSI[a],i,j] 
PauliChainFactor[%]
