(* ::Package:: *)

 


(* ::Section:: *)
(*CompleteSquare*)


(* ::Text:: *)
(*`CompleteSquarep[exp, x]` completes the square of a second order polynomial in the momentum x.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md).*)


(* ::Subsection:: *)
(*Examples*)


CompleteSquare[4 SP[p]+ SP[b,p] +c,p]


CompleteSquare[4 SP[p]+ SP[b,p] +c,p,q]


ex1=5 SP[2p+3r,p+r]


ex2=CompleteSquare[ex1,p]


ex1-ex2//ScalarProductExpand//Expand


CompleteSquare[5 SP[2p+3r,p+r],p,q]


SPD[a]+2SPD[a,b]

ex=CompleteSquare[%,a]


ex//StandardForm
