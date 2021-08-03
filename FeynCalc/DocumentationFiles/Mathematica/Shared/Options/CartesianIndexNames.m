(* ::Package:: *)

 


(* ::Section:: *)
(*CartesianIndexNames*)


(* ::Text:: *)
(*`CartesianIndexNames` is an option for `FCFAConvert`, `FCCanonicalizeDummyIndices` and other functions. It renames the generic dummy Cartesian indices to the indices in the supplied list.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCFAConvert](FCFAConvert), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices), [CartesianIndexNames](CartesianIndexNames).*)


(* ::Subsection:: *)
(*Examples*)


CLC[i1,i2,i3]CGA[i1,i2,i3]
FCCanonicalizeDummyIndices[%]


CLC[i1,i2,i3]CGA[i1,i2,i3]
FCCanonicalizeDummyIndices[%,CartesianIndexNames->{i,j,k}]



