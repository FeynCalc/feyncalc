 
(* ::Section:: *)
(* LC *)
(* ::Text:: *)
(*LC[m, n, r, s] evaluates to 4-dimensional $\epsilon ^{m n r s}$ by virtue of applying FeynCalcInternal. LC[m,...][p, ...] evaluates to 4-dimensional $\epsilon ^{m \text{..} \mu  \text{..}}p_{\mu  \text{..}}$ applying FeynCalcInternal..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Eps, LCD.*)



(* ::Subsection:: *)
(* Examples *)



LC[\[Mu],\[Nu],\[Rho],\[Sigma]]

%//FCI

%//StandardForm

LC[\[Mu],\[Nu]][p,q]

%//FCI//StandardForm

Contract[LC[\[Mu],\[Nu],\[Rho]][p] LC[\[Mu],\[Nu],\[Rho]][q]] 
