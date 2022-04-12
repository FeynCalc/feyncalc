(* ::Package:: *)

 


(* ::Section:: *)
(*MomentumExpand*)


(* ::Text:: *)
(*`MomentumExpand[expr]` expands `Momentum[a+b+ ...]` in `expr` into `Momentum[a] + Momentum[b] + ...`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [MomentumCombine](MomentumCombine.md).*)


(* ::Subsection:: *)
(*Examples*)


MomentumExpand[Momentum[p+q]]//StandardForm


ScalarProduct[p+q,r]


ScalarProduct[p+q,r]//StandardForm


ex=MomentumExpand[ScalarProduct[p+q,r]]


ex//StandardForm


ex=MomentumExpand[ScalarProduct[p+q,r-p]]


ex//StandardForm
