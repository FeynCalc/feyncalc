 
(* ::Section:: *)
(* A0 *)
(* ::Text:: *)
(*A0[m^2] is the Passarino-Veltman one-point integral $A_0.$.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*B0, C0, D0, PaVe.*)



(* ::Subsection:: *)
(* Examples *)



Options[ A0]


(* ::Text:: *)
(*By default $A_0$ is not expressed in terms of $B_0$.*)


A0[m^2]

SetOptions[A0,A0ToB0->True];
A0[m^2]

SetOptions[A0,A0ToB0->False];

(* ::Text:: *)
(*According to the rules of dimensional regularization $A_0(0)$ is set to 0.*)


A0[0]

A0[SmallVariable[M^2]]
