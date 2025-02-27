(* ::Package:: *)

 


(* ::Section:: *)
(*Expanding and undoing expansions*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Manipulations*)


(* ::Text:: *)
(*FeynCalc offers further useful functions for the manipulations of Lorentz tensors and Dirac matrices. To expand scalar products*)


ex1=SP[p+q+r,s+t]


(* ::Text:: *)
(*or expressions like*)


ex2=FV[p+q+r,\[Mu]]


(* ::Text:: *)
(*one can use*)


ExpandScalarProduct[ex1]


ExpandScalarProduct[ex2]


(* ::Text:: *)
(*Notice that `ExpandScalarProduct` can also do expansions only for the given momentum, while*)
(*leaving the rest of the expression untouched, e.g.*)


x SP[p1+p2,q1+q2]+y SP[p3+p4,q3+q4]+z SP[p5+p6,q5+q6]
ExpandScalarProduct[%,Momentum->{p1}]


(* ::Text:: *)
(*For the expansion of `Eps` tensors, we use*)


LC[][p1+p2,q,r,s]
EpsEvaluate[%]


(* ::Text:: *)
(*`EpsEvaluate` also reorders the arguments of `Eps` according to its antisymmetric properties*)


LC[\[Mu],\[Sigma],\[Rho],\[Nu]]
EpsEvaluate[%]


(* ::Text:: *)
(*The inverse of `ExpandScalarProduct` is called `MomentumCombine`*)


3FV[p,\[Mu]]+4FV[q,\[Mu]]
MomentumCombine[%]


(* ::Text:: *)
(*This also works for scalar products, but the results may not be always optimal*)


SP[p+q+t,r+s]
ExpandScalarProduct[%]
MomentumCombine[%]


SP[p+q+t,r+s]+SP[r,s]
ExpandScalarProduct[%]
MomentumCombine[%]


(* ::Text:: *)
(*For Dirac matrices the corresponding functions are `DiracGammaExpand` and `DiracGammaCombine`*)


GA[\[Mu]] . GS[p+q] . GA[\[Nu]] . GS[r+s]
DiracGammaExpand[%]
DiracGammaCombine[%]


(* ::Text:: *)
(*Notice the `DiracGammaExpand` does not expand the whole noncommutative product. If you need that, use `DotSimplify`*)


GA[\[Mu]] . GS[p+q] . GA[\[Nu]] . GS[r+s]
%//DiracGammaExpand//DotSimplify
