(* ::Package:: *)

 


(* ::Section:: *)
(*SUNDelta*)


(* ::Text:: *)
(*`SUNDelta[a, b]`  is the Kronecker-delta for $SU(N)$ with color indices `a` and `b` in the adjoint representation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitSUNIndex](ExplicitSUNIndex.md), [SD](SD.md), [SUNF](SUNF.md), [SUNIndex](SUNIndex.md), [SUNSimplify](SUNSimplify.md), [Trick](Trick.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNDelta[SUNIndex[a],SUNIndex[b]]


SUNDelta[SUNIndex[a],SUNIndex[b]]SUNDelta[SUNIndex[b],SUNIndex[c]]
SUNSimplify[%]


SUNDelta[SUNIndex[a],SUNIndex[b]]
%//StandardForm


SUNDelta[SUNIndex[a],SUNIndex[b]]//FCI//FCE//StandardForm


SD[a,b]//FCI//StandardForm


(* ::Text:: *)
(*The arguments of `SUNDelta` may also represent explicit integer indices via the head `ExplictiSUNIndex`. The difference is that `SUNSimplify` will only sum over symbolic indices.*)


SUNDelta[SUNIndex[a],ExplicitSUNIndex[2]]SUNDelta[SUNIndex[a],SUNIndex[b]]*
SUNDelta[SUNIndex[c],ExplicitSUNIndex[2]]//SUNSimplify


%//StandardForm


SD[1,2]//FCI//StandardForm
