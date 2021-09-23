(* ::Package:: *)

 


(* ::Section:: *)
(*LC*)


(* ::Text:: *)
(*`LC[m, n, r, s]` evaluates to 4-dimensional $\varepsilon^{m n r s}$ by virtue of applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*`LC[m,...][p, ...]` evaluates to 4-dimensional $\epsilon ^{m \ldots  \mu  \ldots}p_{\mu  \ldots}$ applying `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [LCD](LCD.md).*)


(* ::Subsection:: *)
(*Examples*)


LC[\[Mu],\[Nu],\[Rho],\[Sigma]]
%//FCI
%//StandardForm


LC[\[Mu],\[Nu]][p,q]
%//FCI//StandardForm


Contract[LC[\[Mu],\[Nu],\[Rho]][p] LC[\[Mu],\[Nu],\[Rho]][q]] 
