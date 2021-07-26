(* ::Package:: *)

(* ::Section:: *)
(*MomentumExpand*)


(* ::Text:: *)
(*`MomentumExpand[expr]` expands `Momentum[a+b+ ...]` in `expr` into `Momentum[a] + Momentum[b] + ...`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[ExpandScalarProduct](ExpandScalarProduct), [MomentumCombine](MomentumCombine).*)


(* ::Subsection:: *)
(*Examples*)


MomentumExpand[Momentum[p+q]]//StandardForm
ScalarProduct[p+q,r]
%//StandardForm


MomentumExpand[ScalarProduct[p+q,r]]
%//StandardForm


MomentumExpand[ScalarProduct[p+q,r-p]]
%//StandardForm



