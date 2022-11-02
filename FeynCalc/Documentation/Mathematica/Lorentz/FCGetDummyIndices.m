(* ::Package:: *)

 


(* ::Section:: *)
(*FCGetDummyIndices*)


(* ::Text:: *)
(*`FCGetDummyIndices[exp, {head1, head2, ...}]` returns the list of dummy indices from heads `head1`, `head2`, ...*)


(* ::Text:: *)
(*As always in FeynCalc, Einstein summation convention is implicitly assumed.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md), [Contract](Contract.md), *)
(*[DummyIndexFreeQ](DummyIndexFreeQ.md), [FCGetFreeIndices](FCGetFreeIndices.md), *)
(*[FreeIndexFreeQ](FreeIndexFreeQ.md).*)


(* ::Subsection:: *)
(*Examples*)


FCI[FV[p,\[Mu]] FV[q,\[Nu]]]

FCGetDummyIndices[%,{LorentzIndex}]


FCI[FV[p,\[Mu]] FV[q,\[Mu]]]

FCGetDummyIndices[%,{LorentzIndex}]


FCI[SUNT[a,b]]

FCGetDummyIndices[%,{SUNIndex}]


FCI[SUNT[a,a]]

FCGetDummyIndices[%,{SUNIndex}]



