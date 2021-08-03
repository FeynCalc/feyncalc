(* ::Package:: *)

 


(* ::Section:: *)
(*Rename*)


(* ::Text:: *)
(*`Rename` is an option for `Contract`. If set to `True`, dummy indices in `Eps` are renamed, using `$MU[i]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Contract](Contract).*)


(* ::Subsection:: *)
(*Examples*)


LC[\[Mu],\[Nu],\[Rho],\[Sigma]]LC[\[Alpha],\[Nu],\[Rho],\[Sigma]]
Contract[%,EpsContract->False,Rename->True]



