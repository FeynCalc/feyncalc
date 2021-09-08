(* ::Package:: *)

 


(* ::Section:: *)
(*TemporalMomentum*)


(* ::Text:: *)
(*`TemporalMomentum[p]`  is the head of the temporal component of a $4$-momentum $p^0$. The internal representation of the temporal component $p^0$ is `TemporalMomentum[p]`.*)


(* ::Text:: *)
(*`TemporalMomentum` may appear only inside `TemporalPair`s.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [TemporalPair](TemporalPair.md), [ExplicitLorentzIndex](ExplicitLorentzIndex.md).*)


(* ::Subsection:: *)
(*Examples*)


TemporalMomentum[p]


TemporalMomentum[-q]
%//StandardForm


TemporalMomentum[p+q]
%//MomentumExpand//StandardForm


%//MomentumCombine//StandardForm
