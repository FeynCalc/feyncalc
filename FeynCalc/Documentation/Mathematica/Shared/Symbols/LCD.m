(* ::Package:: *)

 


(* ::Section:: *)
(*LCD*)


(* ::Text:: *)
(*`LCD[m, n, r, s]` evaluates to $D$-dimensional $\varepsilon^{m n r s}$ by virtue of applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*`LCD[m,...][p, ...]` evaluates to $D$-dimensional $\epsilon ^{m \ldots \mu \ldots}p_{\mu \ldots}$ applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*When some indices of a Levi-Civita-tensor are contracted with 4-vectors, FeynCalc suppresses explicit dummy indices by putting those vectors into the corresponding index slots. For example,  $\varepsilon^{p_1 p_2 p_3 p_4}$ (accessible via `LCD[][p1,p2,p3,p4]`) correspond to $\varepsilon_{\mu \nu \rho \sigma} p_1^\mu p_2^\nu p_3^\rho p_4^\sigma$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [LC](LC.md).*)


(* ::Subsection:: *)
(*Examples*)


LCD[\[Mu],\[Nu],\[Rho],\[Sigma]]


LCD[\[Mu],\[Nu],\[Rho],\[Sigma]]//FCI//StandardForm


LCD[\[Mu],\[Nu]][p,q]


LCD[\[Mu],\[Nu]][p,q]//FCI//StandardForm


Factor2[Contract[LCD[\[Mu],\[Nu],\[Rho]][p] LCD[\[Mu],\[Nu],\[Rho]][q]]]


LCD[\[Mu],\[Nu],\[Rho],\[Sigma]]FVD[Subscript[p, 1],\[Mu]]FVD[Subscript[p, 2],\[Nu]]FVD[Subscript[p, 3],\[Rho]]FVD[Subscript[p, 4],\[Sigma]]

Contract[%]



