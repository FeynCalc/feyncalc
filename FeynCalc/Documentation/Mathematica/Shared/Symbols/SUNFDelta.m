(* ::Package:: *)

 


(* ::Section:: *)
(*SUNFDelta*)


(* ::Text:: *)
(*`SUNFDelta[a, b]` is the Kronecker-delta for $SU(N)$ with color indices `a` and `b` in the fundamental representation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNDelta](SUNDelta.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNFDelta[SUNFIndex[a],SUNFIndex[b]]


SUNFDelta[SUNFIndex[a],SUNFIndex[b]]SUNFDelta[SUNFIndex[b],SUNFIndex[c]]
%//SUNSimplify


SUNFDelta[SUNFIndex[a],SUNFIndex[b]]^2
%//SUNSimplify


SUNFDelta[SUNFIndex[a],SUNFIndex[b]]//StandardForm


SUNFDelta[SUNFIndex[a],SUNFIndex[b]]//FCI//FCE//StandardForm


SDF[a,b]//FCI//StandardForm


(* ::Text:: *)
(*The arguments of `SUNFDelta` may also represent explicit integer indices via the head `ExplictiSUNFIndex`. The difference is that `SUNSimplify` and `SUNFSimplify` will only sum over symbolic indices.*)


SUNFDelta[SUNFIndex[a],ExplicitSUNFIndex[2]]SUNFDelta[SUNFIndex[a],SUNFIndex[b]]SUNFDelta[SUNFIndex[c],ExplicitSUNFIndex[2]]//SUNFSimplify
%//StandardForm


SDF[1,2]//FCI//StandardForm
