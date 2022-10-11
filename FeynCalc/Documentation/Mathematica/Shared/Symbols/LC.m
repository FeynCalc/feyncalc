(* ::Package:: *)

 


(* ::Section:: *)
(*LC*)


(* ::Text:: *)
(*`LC[m, n, r, s]` evaluates to 4-dimensional $\varepsilon^{m n r s}$ by virtue of applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*`LC[m,...][p, ...]` evaluates to 4-dimensional $\epsilon ^{m \ldots  \mu  \ldots}p_{\mu  \ldots}$ applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*When some indices of a Levi-Civita-tensor are contracted with 4-vectors, FeynCalc suppresses explicit dummy indices by putting those vectors into the corresponding index slots. For example,  $\varepsilon^{p_1 p_2 p_3 p_4}$ (accessible via `LC[][p1,p2,p3,p4]`) correspond to $\varepsilon_{\mu \nu \rho \sigma} p_1^\mu p_2^\nu p_3^\rho p_4^\sigma$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [LCD](LCD.md).*)


(* ::Subsection:: *)
(*Examples*)


LC[\[Mu],\[Nu],\[Rho],\[Sigma]]


LC[\[Mu],\[Nu],\[Rho],\[Sigma]]//FCI//StandardForm


LC[\[Mu],\[Nu]][p,q]


LC[\[Mu],\[Nu]][p,q]//FCI//StandardForm


Contract[LC[\[Mu],\[Nu],\[Rho]][p] LC[\[Mu],\[Nu],\[Rho]][q]] 


LC[\[Mu],\[Nu],\[Rho],\[Sigma]]FV[Subscript[p, 1],\[Mu]]FV[Subscript[p, 2],\[Nu]]FV[Subscript[p, 3],\[Rho]]FV[Subscript[p, 4],\[Sigma]]

Contract[%]



