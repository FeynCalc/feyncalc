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
(*[Eps](Eps), [LC](LC).*)


(* ::Subsection:: *)
(*Examples*)


LCD[\[Mu],\[Nu],\[Rho],\[Sigma]]
%//FCI
%//StandardForm


LCD[\[Mu],\[Nu]][p,q]
%//FCI//StandardForm


Factor2[Contract[LCD[\[Mu],\[Nu],\[Rho]][p] LCD[\[Mu],\[Nu],\[Rho]][q]]]
