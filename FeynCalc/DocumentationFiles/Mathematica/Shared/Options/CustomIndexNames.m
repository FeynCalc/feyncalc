(* ::Package:: *)

 


(* ::Section:: *)
(*CustomIndexNames*)


(* ::Text:: *)
(*`CustomIndexNames` is an option of `FCCanonicalizeDummyIndices`. It allows to specify custom names for canonicalized dummy indices of custom index heads.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFAConvert](FCFAConvert.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [LorentzIndexNames](LorentzIndexNames.md), [CartesianIndexNames](CartesianIndexNames.md).*)


(* ::Subsection:: *)
(*Examples*)


ex=T1[MyIndex2[a], MyIndex1[b]] v1[MyIndex1[b]] v2[MyIndex2[a]] + 
T1[MyIndex2[c], MyIndex1[f]] v1[MyIndex1[f]] v2[MyIndex2[c]]


FCCanonicalizeDummyIndices[ex , Head -> {MyIndex1, MyIndex2}, 
CustomIndexNames -> {{MyIndex1, {i1}}, {MyIndex2, {i2}}}]
