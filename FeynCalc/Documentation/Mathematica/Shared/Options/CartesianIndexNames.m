(* ::Package:: *)

 


(* ::Section:: *)
(*CartesianIndexNames*)


(* ::Text:: *)
(*`CartesianIndexNames` is an option for `FCFAConvert`, `FCCanonicalizeDummyIndices` and other functions. It renames the generic dummy Cartesian indices to the indices in the supplied list.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFAConvert](FCFAConvert.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [CartesianIndexNames](CartesianIndexNames.md).*)


(* ::Subsection:: *)
(*Examples*)


CLC[i1,i2,i3]CGA[i1,i2,i3]
FCCanonicalizeDummyIndices[%]


CLC[i1,i2,i3]CGA[i1,i2,i3]
FCCanonicalizeDummyIndices[%,CartesianIndexNames->{i,j,k}]



