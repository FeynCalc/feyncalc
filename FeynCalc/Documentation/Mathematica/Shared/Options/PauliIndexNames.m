(* ::Package:: *)

 


(* ::Section:: *)
(*PauliIndexNames*)


(* ::Text:: *)
(*`PauliIndexNames` is an option for `FCFAConvert`, `FCCanonicalizeDummyIndices` and other functions. It renames the generic dummy Pauli indices to the indices in the supplied list.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFAConvert](FCFAConvert.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [DiracIndexNames](DiracIndexNames.md), [LorentzIndexNames](LorentzIndexNames.md).*)


(* ::Subsection:: *)
(*Examples*)


PCHN[CSI[a],i,j]PCHN[CSI[b],j,k]
FCCanonicalizeDummyIndices[%]


PCHN[CSI[a],i,j]PCHN[CSI[b],j,k]
FCCanonicalizeDummyIndices[%,PauliIndexNames->{l}]



