(* ::Package:: *)

 


(* ::Section:: *)
(*Momentum*)


(* ::Text:: *)
(*`Momentum[p]` is the head of a four momentum `p`.*)


(* ::Text:: *)
(*The internal representation of a $4$-dimensional $p$ is `Momentum[p]`.*)


(* ::Text:: *)
(*For other than $4$ dimensions: `Momentum[p, dim]`.*)


(* ::Text:: *)
(*`Momentum[p, 4]` simplifies to `Momentum[p]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [Eps](Eps.md), [LorentzIndex](LorentzIndex.md), [MomentumExpand](MomentumExpand.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This is a $4$-dimensional momentum.*)


Momentum[p]


(* ::Text:: *)
(*As an optional second argument the dimension must be specified if it is different from $4$.*)


Momentum[p,D]


(* ::Text:: *)
(*The dimension index is suppressed in the output.*)


Momentum[p,d]


Momentum[-q]
%//StandardForm


ex=Momentum[p-q] + Momentum[2q]
%//StandardForm


ex//MomentumExpand//StandardForm


ex//MomentumCombine//StandardForm


ChangeDimension[Momentum[p],d]//StandardForm
