(* ::Package:: *)

 


(* ::Section:: *)
(*FCDiffEqChangeVariables*)


(* ::Text:: *)
(*`FCDiffEqChangeVariables[mat, x, y, rule, yOfX]` applies a variable transformation from `x` to `y`described by `rule`, where `yOfX` denotes $y(x)$. Here `mat` is a matrix in the context of differential equations, i.e. it can be either the matrix $\mathcal{A}$ or $\mathcal{B}$ from the pre-canonical $F' = \mathcal{A} F$ or canonical $G' = \varepsilon \mathcal{B} G$  form, or the transformation matrix $\mathcal{T}$ with $F = \mathcal{T} G$ .*)


(* ::Text:: *)
(*By default, the transformation also includes the prefactor $1/f'(y)$. This is correct for $\mathcal{A}$ or $\mathcal{B}$ but not for $\mathcal{T}$ matrices. The inclusion of the prefactor can be disabled by setting the option `Prefactor` to `False`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Examples*)


mat={{(-2*(-1 + eps))/x, 0, 0, 0}, {0, (1 - eps)/x, 0, 0}, {0, (-2*(-1 + eps))/(x*(-1 + 4*x)), 
(-2*(-1 + 2*eps))/(-1 + 4*x), 0},  {(-2*(-1 + eps))/(x*(-1 + 4*x)), 0, 0, 
(-1 + eps + 6*x - 8*eps*x)/(x*(-1 + 4*x))}}


matNew=FCDiffEqChangeVariables[mat,x, y, x -> (1 - y^2)/4, Sqrt[1 - 4*x],Assumptions->{y>0}]


(* ::Text:: *)
(*Setting the option `Reverse` to `True` allows to undo the transformation.*)


matCheck=FCDiffEqChangeVariables[matNew,x, y, x -> (1 - y^2)/4, Sqrt[1 - 4*x],Reverse->True]


Simplify[matCheck-mat]//Flatten//Union


FCDiffEqChangeVariables[mat,x, y, x -> (1 - y^2)/4, Sqrt[1 - 4*x],Assumptions->{y>0},
Prefactor->False]
