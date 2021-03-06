 
(* ::Section:: *)
(* CLC *)
(* ::Text:: *)
(*CLC[m, n, r] evaluates to Eps[CartesianIndex[m], CartesianIndex[n], CartesianIndex[r]] applying FeynCalcInternal. CLC[m,...][p, ...] evaluates to Eps[CartesianIndex[m], ..., CartesianMomentum[p], ...] applying FeynCalcInternal..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*LC, Eps.*)



(* ::Subsection:: *)
(* Examples *)



CLC[i,j,k]

%//FCI//StandardForm


CLC[i][p,q]

%//FCI//StandardForm

Contract[CLC[i,j,k]CLC[i,l,m]]
