(* ::Package:: *)

 


(* ::Section:: *)
(*CLC*)


(* ::Text:: *)
(*`CLC[m, n, r]` evaluates to `Eps[CartesianIndex[m], CartesianIndex[n], CartesianIndex[r]]` applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*`CLC[m,...][p, ...]` evaluates to `Eps[CartesianIndex[m], ..., CartesianMomentum[p], ...]` applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*When some indices of a Levi-Civita-tensor are contracted with 3-vectors, FeynCalc suppresses explicit dummy indices by putting those vectors into the corresponding index slots. For example,  $\varepsilon^{p_1 p_2 p_3}$ (accessible via `CLC[][p1,p2,p3]`) correspond to $\varepsilon^{i j k} p_1^i p_2^j p_3^k$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [LC](LC.md), [Eps](Eps.md).*)


(* ::Subsection:: *)
(*Examples*)


CLC[i,j,k]


CLC[i,j,k]//FCI//StandardForm


CLC[i][p,q]


CLC[i][p,q]//FCI//StandardForm


Contract[CLC[i,j,k]CLC[i,l,m]]


CLC[i,j,k]CV[Subscript[p, 1],i]CV[Subscript[p, 2],j]CV[Subscript[p, 3],k]

Contract[%]
