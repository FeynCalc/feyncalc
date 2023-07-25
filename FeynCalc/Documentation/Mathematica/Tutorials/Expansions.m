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
(*For the expansion of `Eps` tensors, we use*)


LC[][p1+p2,q,r,s]
EpsEvaluate[%]


(* ::Text:: *)
(*EpsEvaluate also reorders the arguments of Eps according to its antisymmetric properties*)


LC[\[Mu],\[Sigma],\[Rho],\[Nu]]
EpsEvaluate[%]


(* ::Text:: *)
(*The inverse of `ExpandScalarProduct` is called `MomentumCombine`*)


3FV[p,\[Mu]]+4FV[q,\[Mu]]
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
