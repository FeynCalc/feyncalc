(* ::Package:: *)

 


(* ::Section:: *)
(*SDF*)


(* ::Text:: *)
(*`SDF[i, j]` denotes the $SU(N)$ Kronecker delta with color indices `i` and `j` in the fundamental representation. `SDF[i,j]` is transformed into `SUNFDelta[SUNFIndex[i],SUNFIndex[j]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNFDelta](SUNFDelta.md).*)


(* ::Subsection:: *)
(*Examples*)


SDF[a,b]
%//FCI//StandardForm
%//FCE//StandardForm
