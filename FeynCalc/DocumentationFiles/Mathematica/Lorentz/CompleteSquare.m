 
(* ::Section:: *)
(* CompleteSquare *)
(* ::Text:: *)
(*CompleteSquare completes the square of a second order polynomial in the momentum x. CompleteSquare[a $p^2$+b p+c, p] -> -$b^2$/(4 a)+c+a (b/(2 a)+x)^2. CompleteSquare[a $p^2$+b p+c, p, q] -> {-$b^2$/(4 a)+c+a $q^2$, q->b/(2 a)+p}..*)


(* ::Subsection:: *)
(* Examples *)
5SP[2p+3r,p+r]

CompleteSquare[%,p]

%-%%//ScalarProductExpand//Expand

CompleteSquare[5SP[2p+3r,p+r],p,q]
