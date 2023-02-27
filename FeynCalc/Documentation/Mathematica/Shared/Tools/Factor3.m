(* ::Package:: *)

 


(* ::Section:: *)
(*Factor3*)


(* ::Text:: *)
(*`Factor3[exp]` factors a rational function `exp` over the field of complex numbers.*)


(* ::Text:: *)
(*`Factor3` is primarily meant to be used on matrices from differential equations and Feynman parametric*)
(*representations of loop integrals. Its main goal is to rewrite all denominators such, that they can be integrated in terms of HPLs or GPLs (when possible).*)


(* ::Text:: *)
(*To avoid performance bottlenecks, in the case of rational functions only the denominator will be factored by default. This can be changed by setting the option `Numerator` to `True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCPartialFractionForm](FCPartialFractionForm.md).*)


(* ::Subsection:: *)
(*Examples*)


Factor3[(1-4x)(1+3y)]


Factor3[16*(1-2*eps)^2*x^2]


Factor3[2*(32904490323+164521613783*eps+1256744*eps^2)*(11-5*eps-47*eps^2+44*eps^3)]


mat={{(2 - 2*eps)/x, 0, 0, 0, 0}, {0, (2 - 2*eps)/(2*x), 0, 0, 0}, 
{0, (-2 + 2*eps)/(x - 4*x^2), (6 - 2*(4 - 2*eps))/(1 - 4*x), 0, 0}, 
{(-2 + 2*eps)/(x - 4*x^2), 0, 0, (2 - 2*eps + 4*(5 - 2*(4 - 2*eps))*x)/(2*(1 - 
4*x)*x), 0},  {(2 - 2*eps)^2/(16*(1 - x)*x^2), -1/8*(2 - 2*eps)^2/((1 - x)*x^2), 
0, 0, -((7 - 2*(4 - 2*eps) - 13*x + 4*(4 - 2*eps)*x)/(2*x - 2*x^2))}};


Factor3[mat]
