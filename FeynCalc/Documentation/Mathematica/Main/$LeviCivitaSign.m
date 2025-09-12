(* ::Package:: *)

 


(* ::Section:: *)
(*$LeviCivitaSign*)


(* ::Text:: *)
(*`$LeviCivitaSign` is a global variable that determines the sign in the result of a Dirac trace of four gamma matrices and $\gamma^5$.  `$LeviCivitaSign` is by default set to `-1` which corresponds to the convention `TR[GA[a,b,c,d,5]] = -4*I*Eps[a,b,c,d]`. Setting `$LeviCivitaSign=-I` will switch to the FORM-convention.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [LC](LC.md), [Eps](Eps.md), [DiracTrace](DiracTrace.md).*)


(* ::Subsection:: *)
(*Examples*)


$LeviCivitaSign

DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma],5]]//DiracSimplify


(* ::Text:: *)
(*This sets the same convention as in FORM*)


$LeviCivitaSign=-I;

DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma],5]]//DiracSimplify


(* ::Text:: *)
(*Back to the standard value*)


$LeviCivitaSign=-1;
