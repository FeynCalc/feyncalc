(* ::Package:: *)

 


(* ::Section:: *)
(*InsideDiracTrace*)


(* ::Text:: *)
(*`InsideDiracTrace` is an option of `DiracSimplify` and some other functions dealing with Dirac algebra. If set to `True`, the function assumes to operate inside a Dirac trace, i.e., products of an odd number of Dirac matrices are discarded. For more details, see the documentation for `DiracSimplify`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


DiracSimplify[GA[\[Mu],\[Nu],\[Rho]]]


(* ::Text:: *)
(*A trace of an 3 Dirac matrices vanishes:*)


DiracSimplify[GA[\[Mu],\[Nu],\[Rho]],InsideDiracTrace->True]
