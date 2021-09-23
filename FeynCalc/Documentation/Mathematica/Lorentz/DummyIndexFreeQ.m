(* ::Package:: *)

 


(* ::Section:: *)
(*DummyIndexFreeQ*)


(* ::Text:: *)
(*`DummyIndexFreeQ[exp, {head1, head2, ...}]` returns `True` if the expression contains dummy indices with heads `head1`, `head2`, ... and `False` otherwise.*)


(* ::Text:: *)
(*As always in FeynCalc, Einstein summation convention is implicitly assumed.*)


(* ::Text:: *)
(*The function is optimized for large expressions, i.e. it is not so good as a criterion in e.g. Select.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md), [Contract](Contract.md), [FreeIndexFreeQ](FreeIndexFreeQ.md).*)


(* ::Subsection:: *)
(*Examples*)


FCI[FV[p,\[Mu]] FV[q,\[Nu]]]
DummyIndexFreeQ[%,{LorentzIndex}]


FCI[FV[p,\[Mu]] FV[q,\[Mu]]]
DummyIndexFreeQ[%,{LorentzIndex}]


FCI[SUNT[a,b]]
DummyIndexFreeQ[%,{SUNIndex}]


FCI[SUNT[a,a]]
DummyIndexFreeQ[%,{SUNIndex}]



