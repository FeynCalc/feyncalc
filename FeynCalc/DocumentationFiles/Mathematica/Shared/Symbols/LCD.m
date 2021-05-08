 
(* ::Section:: *)
(* LCD *)
(* ::Text:: *)
(*LCD[m, n, r, s] evaluates to D-dimensional $\epsilon ^{m n r s}$ by virtue of FeynCalcInternal. LCD[m,...][p, ...] evaluates to D-dimensional $\epsilon ^{m \text{..} \mu  \text{..}}p_{\mu  \text{..}}$ applying FeynCalcInternal..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Eps, LC.*)



(* ::Subsection:: *)
(* Examples *)



LCD[\[Mu],\[Nu],\[Rho],\[Sigma]]

%//FCI//StandardForm

LCD[\[Mu],\[Nu]][p,q]

%//FCI//StandardForm

Factor2[Contract[LCD[\[Mu],\[Nu],\[Rho]][p] LCD[\[Mu],\[Nu],\[Rho]][q]]]
