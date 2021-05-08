 
(* ::Section:: *)
(* SP *)
(* ::Text:: *)
(*SP[a, b] denotes a four-dimensional scalar product. SP[a, b] is transformed into ScalarProduct[a, b] by FeynCalcInternal. SP[p] is the same as SP[p, p] $\left(=p^2\right.$)..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Calc, ExpandScalarProduct, ScalarProduct.*)



(* ::Subsection:: *)
(* Examples *)



SP[p,q] + SP[q]

SP[p-q,q+2p]

Calc[ SP[p-q,q+2p] ]

ExpandScalarProduct[SP[p-q]]

SP[a,b]//StandardForm

SP[a,b]//FCI//StandardForm

SP[a,b]//FCI//FCE//StandardForm
