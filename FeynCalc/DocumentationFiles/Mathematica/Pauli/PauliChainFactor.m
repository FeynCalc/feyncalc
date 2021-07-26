(* ::Package:: *)

 


(* ::Section:: *)
(*PauliChainFactor*)


(* ::Text:: *)
(*`PauliChainFactor[exp]` factors out all expressions inside a `PauliChain` to which the chain doesn't apply. For example, all objects that are not Pauli matrices can be safely factored out from every Pauli chain.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[PauliChain](PauliChain), [PCHN](PCHN), [PauliIndex](PauliIndex), [PauliIndexDelta](PauliIndexDelta), [DIDelta](DIDelta), [PauliChainJoin](PauliChainJoin), [PauliChainCombine](PauliChainCombine), [PauliChainExpand](PauliChainExpand).*)


(* ::Subsection:: *)
(*Examples*)


PCHN[CV[p,\[Nu]]CSI[a] . CSI[b] . CSI[a],i,j] 
PauliChainFactor[%]
