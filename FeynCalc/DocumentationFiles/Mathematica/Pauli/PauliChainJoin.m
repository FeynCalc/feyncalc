(* ::Package:: *)

 


(* ::Section:: *)
(*PauliChainJoin*)


(* ::Text:: *)
(*`PauliChainJoin[exp]` joins chains of Pauli matrices with explicit Pauli indices wrapped with a head `PauliChain`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[PauliChain](PauliChain), [PCHN](PCHN), [PauliIndex](PauliIndex), [PauliIndexDelta](PauliIndexDelta), [DIDelta](DIDelta), [PauliChainCombine](PauliChainCombine), [PauliChainExpand](PauliChainExpand), [PauliChainFactor](PauliChainFactor).*)


(* ::Subsection:: *)
(*Examples*)


PCHN[PauliXi[-I],i] PCHN[CSID[a] . CSID[b],i,j] PCHN[j,PauliEta[I]]
PauliChainJoin[%]
