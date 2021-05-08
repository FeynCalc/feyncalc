 
(* ::Section:: *)
(* SPD *)
(* ::Text:: *)
(*SPD[a, b] denotes a D-dimensional scalar product. SPD[a, b] is transformed into ScalarProduct[a, b,Dimension->D] by FeynCalcInternal. SPD[p] is the same as SPD[p,p] $\left(=p^2\right.$)..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*PD, Calc, ExpandScalarProduct, ScalarProduct.*)



(* ::Subsection:: *)
(* Examples *)



SPD[p,q] + SPD[q]

SPD[p-q,q+2p]

Calc[ SPD[p-q,q+2p] ]

ExpandScalarProduct[SPD[p-q]]

SPD[a,b]//StandardForm

SPD[a,b]//FCI//StandardForm

SPD[a,b]//FCI//FCE//StandardForm

FCE[ChangeDimension[SP[p,q], D]]//StandardForm
