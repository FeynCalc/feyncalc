 
(* ::Section:: *)
(* ScalarProduct *)
(* ::Text:: *)
(*ScalarProduct[p, q]  is the input for the scalar product of two Lorentz vectors p and q. ScalarProduct[p] is equivalent to ScalarProduct[p, p]. Expansion of sums of momenta in ScalarProduct is done with ExpandScalarProduct. Scalar products may be set, e.g. via ScalarProduct[a, b] = m^2; but a and b may not contain sums. Note that ScalarProduct[a, b] = m^2 actually sets Lorentzian scalar products in different dimensions specified by the value of the SetDimensions option. It is highly recommended to set ScalarProduct's before any calculation. This improves the performance of FeynCalc...*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Calc, FCClearScalarProducts, ExpandScalarProduct, ScalarProductCancel, Pair, SP, SPD.*)



(* ::Subsection:: *)
(* Examples *)



ScalarProduct[p,q]

ScalarProduct[p+q,-q]

ScalarProduct[p,p]

ScalarProduct[q]

ScalarProduct[p,q]//StandardForm

ScalarProduct[p,q,Dimension->D]//StandardForm

ScalarProduct[Subscript[p, 1],Subscript[p, 2]] = s/2

ExpandScalarProduct[ ScalarProduct[Subscript[p, 1]-q,Subscript[p, 2]-k]]

Calc[ ScalarProduct[Subscript[p, 1]-q,Subscript[p, 2]-k]]

FCClearScalarProducts[]