 
(* ::Section:: *)
(* ExpandScalarProduct *)
(* ::Text:: *)
(*ExpandScalarProduct[expr] expands scalar products of sums of momenta in expr. ExpandScalarProduct does not use Expand on expr..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Calc, MomentumExpand, MomentumCombine.*)



(* ::Subsection:: *)
(* Examples *)



 SP[p1+p2+p3,p4+p5+p6]

%//ScalarProductExpand

SP[p,p-q]

ExpandScalarProduct[%]

FV[p-q,\[Mu]]

ExpandScalarProduct[%]

SPD[p-q,q-r]

ExpandScalarProduct[%]
