(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopPakOrder*)


(* ::Text:: *)
(*`FCLoopPakOrder[poly, {x1, x2, ...}]` determines a canonical ordering of the Feynman parameters `x1, x2, ...` in the polynomial `poly`.*)


(* ::Text:: *)
(*The function uses the algorithm of Alexey Pak [arXiv:1111.0868](https://arxiv.org/abs/1111.0868). Cf. also the PhD thesis of Jens Hoff [10.5445/IR/1000047447](https://doi.org/10.5445/IR/1000047447) for the detailed description of a possible implementation.*)


(* ::Text:: *)
(*The current implementation is based on the `PolyOrdering` function from FIRE 6 [arXiv:1901.07808](https://arxiv.org/abs/1901.07808)*)


(* ::Text:: *)
(*The function can also directly perform the renaming of the Feynman parameter variables returning the original polynomial in the canonical form. This is done by setting the option `Rename` to `True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopPakOrder](FCLoopPakOrder.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Subsubsection:: *)
(*Canonicalizing a polynomial*)


(* ::Text:: *)
(*Let us consider the following product of `U` and `F` polynomials of some loop integral*)


poly=(x[1]*x[2] + x[1]*x[3] + x[2]*x[3] + x[2]*x[4] + x[3]*x[4] + x[1]*x[5] + x[2]*x[5] + x[4]*x[5])*
 (m1^2*x[1]^2*x[2] + m3^2*x[1]*x[2]^2 + m1^2*x[1]^2*x[3] + m1^2*x[1]*x[2]*x[3] + m2^2*x[1]*x[2]*x[3] + 
  m3^2*x[1]*x[2]*x[3] + m3^2*x[2]^2*x[3] + m2^2*x[1]*x[3]^2 + m2^2*x[2]*x[3]^2 + m1^2*x[1]*x[2]*x[4] - 
  SPD[q, q]*x[1]*x[2]*x[4] + m3^2*x[2]^2*x[4] + m1^2*x[1]*x[3]*x[4] - SPD[q, q]*x[1]*x[3]*x[4] + 
  m2^2*x[2]*x[3]*x[4] + m3^2*x[2]*x[3]*x[4] - SPD[q, q]*x[2]*x[3]*x[4] + m2^2*x[3]^2*x[4] + m1^2*x[1]^2*x[5] + 
  m1^2*x[1]*x[2]*x[5] + m3^2*x[1]*x[2]*x[5] - SPD[q, q]*x[1]*x[2]*x[5] + m3^2*x[2]^2*x[5] + m2^2*x[1]*x[3]*x[5] - 
  SPD[q, q]*x[1]*x[3]*x[5] + m2^2*x[2]*x[3]*x[5] - SPD[q, q]*x[2]*x[3]*x[5] + m1^2*x[1]*x[4]*x[5] - 
  SPD[q, q]*x[1]*x[4]*x[5] + m3^2*x[2]*x[4]*x[5] + m2^2*x[3]*x[4]*x[5] - SPD[q, q]*x[3]*x[4]*x[5])


(* ::Text:: *)
(*Using `FCLoopPakOrder` we can obtain a canonical ordering for this polynomial*)


sigma=FCLoopPakOrder[poly, x]


(* ::Text:: *)
(*This output implies that the polynomial will become canonically ordered upon renaming the Feynman parameter variables as follows*)


fpVars=Table[x[i],{i,1,5}]


repRule=Thread[Rule[Extract[fpVars,List/@First[sigma]],fpVars]]


(* ::Text:: *)
(*This way we obtain the canonical form of our polynomial `poly`*)


poly/.repRule


(* ::Subsubsection:: *)
(*Checking equivalence*)


(* ::Text:: *)
(*Let us consider the following two polynomials*)


poly1=-1/4*(x[2]^2*x[3]) - (x[1]^2*x[4])/4 - 
(x[1]^2*x[5])/4 + (x[1]*x[2]*x[5])/2 - (x[2]^2*x[5])/4 + x[3]*x[4]*x[5]


poly2=-1/4*(x[1]^2*x[2]) - (x[1]^2*x[3])/4 + 
x[2]*x[3]*x[4] + (x[1]*x[3]*x[5])/2 - (x[3]*x[5]^2)/4 - (x[4]*x[5]^2)/4


(* ::Text:: *)
(*These polynomials are not identical*)


poly1===poly2


(* ::Text:: *)
(*However, one can easily recognize that they are actually the same upon renaming Feynman parameters*)
(*`x[i]` in a suitable way. `FCLoopPakOrder` can do such renamings automatically*)


canoPoly1=FCLoopPakOrder[poly1,x,Rename->True]
canoPoly2=FCLoopPakOrder[poly2,x,Rename->True]


(* ::Text:: *)
(*When comparing the canonicalized versions of both polynomials we see that they are indeed identical*)


canoPoly1===canoPoly2
