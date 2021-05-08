 
(* ::Section:: *)
(* DeltaFunction *)
(* ::Text:: *)
(*DeltaFunction[x] is the Dirac delta-function $\delta (x)$.Mathematica also provides a built-in function $text{DiracDelta}$ with comparable properties..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Convolute, DeltaFunctionPrime, Integrate2, SimplifyDeltaFunction.*)



(* ::Subsection:: *)
(* Examples *)



DeltaFunction[1-x]

Integrate2[DeltaFunction[1-x] f[x],{x,0,1}]

Integrate2[DeltaFunction[x] f[x],{x,0,1}]

Integrate2[DeltaFunction[1-x] f[x],{x,0,1}]

Convolute[DeltaFunction[1-x],x]/.FCGV[z_]:>ToExpression[z]
