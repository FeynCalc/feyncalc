(* ::Package:: *)

 


(* ::Section:: *)
(*FCGetFreeIndices*)


(* ::Text:: *)
(*`FCGetFreeIndices[exp, {head1, head2, ...}]`  returns the list of free (uncontracted) indices from heads `head1`, `head2`, ...*)


(* ::Text:: *)
(*As always in FeynCalc, Einstein summation convention is implicitly assumed. The function is optimized for large expressions, i.e. it is not so good as a criterion in e.g. `Select`.*)


(* ::Text:: *)
(*If it is understood that each term in the expression contains the same number of free indices, setting the option `First` to `True` can considerably speed up the evaluation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md), [Contract](Contract.md),*)
(*[FCGetDummyIndices](FCGetDummyIndices.md), [DummyIndexFreeQ](DummyIndexFreeQ.md), [FreeIndexFreeQ](FreeIndexFreeQ.md).*)


(* ::Subsection:: *)
(*Examples*)


FCI[FV[p,\[Mu]] FV[q,\[Nu]]]

FCGetFreeIndices[%,{LorentzIndex}]


FCI[FV[p,\[Mu]] FV[q,\[Mu]]]

FCGetFreeIndices[%,{LorentzIndex}]


FCI[SUNT[a,b]]

FCGetFreeIndices[%,{SUNIndex}]


FCI[SUNT[a,a]]

FCGetFreeIndices[%,{SUNIndex}]
