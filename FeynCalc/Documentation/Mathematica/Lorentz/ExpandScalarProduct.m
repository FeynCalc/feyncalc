(* ::Package:: *)

 


(* ::Section:: *)
(*ExpandScalarProduct*)


(* ::Text:: *)
(*`ExpandScalarProduct[expr]` expands scalar products of sums of momenta in `expr`.*)


(* ::Text:: *)
(*`ExpandScalarProduct` does not use `Expand` on `expr`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Calc](Calc.md), [MomentumExpand](MomentumExpand.md), [MomentumCombine](MomentumCombine.md).*)


(* ::Subsection:: *)
(*Examples*)


SP[p1+p2+p3,p4+p5+p6]
%//ExpandScalarProduct


SP[p,p-q]
ExpandScalarProduct[%]


FV[p-q,\[Mu]]
ExpandScalarProduct[%]


SPD[p-q,q-r]
ExpandScalarProduct[%]


(* ::Text:: *)
(*Using the option `Momentum` one can limit the expansion to particular momenta*)


SP[p1+p2+p3,p4+p5+p6]
ExpandScalarProduct[%,Momentum->{p1}]


(* ::Text:: *)
(*By default `ExpandScalarProduct` does not apply linearity to Levi-Civita tensors*)


LC[\[Mu]][p1+p2,p3+p4,p5+p6]
ExpandScalarProduct[%]


(* ::Text:: *)
(*Using the option `EpsEvaluate` takes care of that*)


LC[\[Mu]][p1+p2,p3+p4,p5+p6]
ExpandScalarProduct[%,EpsEvaluate->True]


(* ::Text:: *)
(*One can use the options `EpsEvaluate` and `Momentum` together*)


LC[\[Mu]][p1+p2,p3+p4,p5+p6]
ExpandScalarProduct[%,EpsEvaluate->True,Momentum->{p1}]


(* ::Text:: *)
(*Of course, the function is also applicable to Cartesian quantities*)


CSP[p1+p2,p3+p4]
ExpandScalarProduct[%]


CLC[][p1+p2,p3+p4,p5+p6]
ExpandScalarProduct[%,EpsEvaluate->True]



