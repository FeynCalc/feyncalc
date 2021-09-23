(* ::Package:: *)

 


(* ::Section:: *)
(*MomentumCombine*)


(* ::Text:: *)
(*`MomentumCombine[expr]` is the inverse operation to `MomentumExpand` and `ExpandScalarProduct`. `MomentumCombine` combines also `Pair`s.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [Momentum](Momentum.md), [MomentumExpand](MomentumExpand.md).*)


(* ::Subsection:: *)
(*Examples*)


Momentum[p]-2 Momentum[q] // MomentumCombine // StandardForm
FV[p,\[Mu]] + 2 FV[q,\[Mu]] 
MomentumCombine[%]
%//StandardForm
%%//ExpandScalarProduct


3 Pair[LorentzIndex[\[Mu]],Momentum[p]]+2 Pair[LorentzIndex[\[Mu]],Momentum[q]]
MomentumCombine[%]
StandardForm[%]



