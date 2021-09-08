(* ::Package:: *)

 


(* ::Section:: *)
(*LorentzIndexNames*)


(* ::Text:: *)
(*`LorentzIndexNames` is an option for `FCFAConvert`, `FCCanonicalizeDummyIndices` and other functions. It renames the generic dummy Lorentz indices to the indices in the supplied list.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFAConvert](FCFAConvert.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [CartesianIndexNames](CartesianIndexNames.md).*)


(* ::Subsection:: *)
(*Examples*)


LC[i1,i2,i3,i4]GA[i1,i2,i3,i4]
FCCanonicalizeDummyIndices[%]


LC[i1,i2,i3,i4]GA[i1,i2,i3,i4]
FCCanonicalizeDummyIndices[%,LorentzIndexNames->{mu,nu,rho,si}]



