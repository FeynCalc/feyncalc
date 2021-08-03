(* ::Package:: *)

 


(* ::Section:: *)
(*CompleteSquare*)


(* ::Text:: *)
(*`CompleteSquarep[exp, x]` completes the square of a second order polynomial in the momentum x.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[ExpandScalarProduct](ExpandScalarProduct).*)


(* ::Subsection:: *)
(*Examples*)


CompleteSquare[4 SP[p]+ SP[b,p] +c,p]


CompleteSquare[4 SP[p]+ SP[b,p] +c,p,q]


5SP[2p+3r,p+r]
CompleteSquare[%,p]
%-%%//ScalarProductExpand//Expand


CompleteSquare[5SP[2p+3r,p+r],p,q]


SPD[a]+2SPD[a,b]
CompleteSquare[%,a]
%//StandardForm



