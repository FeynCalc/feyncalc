(* ::Package:: *)

 


(* ::Section:: *)
(*ScalarProduct*)


(* ::Text:: *)
(*`ScalarProduct[p, q]`  is the input for the scalar product of two Lorentz vectors p and q.*)


(* ::Text:: *)
(*ScalarProduct[p] is equivalent to ScalarProduct[p, p].*)


(* ::Text:: *)
(*Expansion of sums of momenta in `ScalarProduct` is done with `ExpandScalarProduct`.*)


(* ::Text:: *)
(*Scalar products may be set, e.g. via `ScalarProduct[a, b] = m^2`; but `a` and `b` may not contain sums.*)


(* ::Text:: *)
(*Note that `ScalarProduct[a, b] = m^2` actually sets Lorentzian scalar products in different dimensions specified by the value of the `SetDimensions` option. It is highly recommended to set `ScalarProduct`s before any calculation. This improves the performance of FeynCalc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Calc](Calc), [FCClearScalarProducts](FCClearScalarProducts), [ExpandScalarProduct](ExpandScalarProduct), [ScalarProductCancel](ScalarProductCancel), [Pair](Pair), [SP](SP), [SPD](SPD).*)


(* ::Subsection:: *)
(*Examples*)


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
