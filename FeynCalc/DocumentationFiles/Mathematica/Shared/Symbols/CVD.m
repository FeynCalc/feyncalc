 
(* ::Section:: *)
(* CVD *)
(* ::Text:: *)
(*CVD[p, i] is a D-1-dimensional Cartesian vector and is transformed into CartesianPair[CartesianMomentum[p,D], CartesianIndex[i,D]] by FeynCalcInternal..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FVD, Pair, CartesianPair.*)



(* ::Subsection:: *)
(* Examples *)



CVD[p,i]

CVD[p-q,i]

FCI[CVD[p,i]]//StandardForm


(* ::Text:: *)
(*ExpandScalarProduct is used to expand momenta in CVD*)


ExpandScalarProduct[CVD[p-q,i]]
