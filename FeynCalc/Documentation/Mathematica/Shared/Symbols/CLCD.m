(* ::Package:: *)

 


(* ::Section:: *)
(*CLCD*)


(* ::Text:: *)
(*`CLCD[m, n, r]`  evaluates to `Eps[CartesianIndex[m, D-1], CartesianIndex[n, D-1], CartesianIndex[r,D-1]]` applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*`CLC[m,...][p, ...]` evaluates to `Eps[CartesianIndex[m, D-1], ..., CartesianMomentum[p, D-1], ...]` applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*When some indices of a Levi-Civita-tensor are contracted with 3-vectors, FeynCalc suppresses explicit dummy indices by putting those vectors into the corresponding index slots. For example,  $\varepsilon^{p_1 p_2 p_3}$ (accessible via `CLCD[][p1,p2,p3]`) correspond to $\varepsilon^{i j k} p_1^i p_2^j p_3^k$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [LCD](LCD.md), [Eps](Eps.md).*)


(* ::Subsection:: *)
(*Examples*)


CLCD[i,j,k]


CLCD[i,j,k]//FCI//StandardForm


CLCD[i,j][p]


CLCD[i,j][p]//FCI//StandardForm


CLCD[i,j][p]CLCD[i,j][q]//Contract//Factor2


CLCD[i,j,k]CVD[Subscript[p, 1],i]CVD[Subscript[p, 2],j]CVD[Subscript[p, 3],k]

Contract[%]
