(* ::Package:: *)

 


(* ::Section:: *)
(*LCD*)


(* ::Text:: *)
(*`LCD[m, n, r, s]` evaluates to $D$-dimensional $\varepsilon^{m n r s}$ by virtue of applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*`LCD[m,...][p, ...]` evaluates to $D$-dimensional $\epsilon ^{m \ldots  \mu  \ldots}p_{\mu  \ldots}$ applying `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [LC](LC.md).*)


(* ::Subsection:: *)
(*Examples*)


LCD[\[Mu],\[Nu],\[Rho],\[Sigma]]
%//FCI
%//StandardForm


LCD[\[Mu],\[Nu]][p,q]
%//FCI//StandardForm


Factor2[Contract[LCD[\[Mu],\[Nu],\[Rho]][p] LCD[\[Mu],\[Nu],\[Rho]][q]]]
