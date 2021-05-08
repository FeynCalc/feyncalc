 
(* ::Section:: *)
(* EpsContract *)
(* ::Text:: *)
(*EpsContract[exp] handles contractions of two Levi-Civita tensors. It is also an option of Contract and other functions that specifies whether such contractions should be done or not..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Eps, Contract.*)



(* ::Subsection:: *)
(* Examples *)



LCD[\[Mu],\[Nu],\[Rho],\[Sigma]]

EpsContract[% %]//Factor2

Contract[% %]//Factor2

Contract[LCD[\[Mu],\[Nu],\[Rho],\[Sigma]]^2,EpsContract->False]
