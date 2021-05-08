 
(* ::Section:: *)
(* CartesianPropagatorDenominator *)
(* ::Text:: *)
(*CartesianPropagatorDenominator[CartesianMomentum[q1, D - 1] +..., CartesianPair[CartesianMomentum[q1, D - 1], CartesianMomentum[p1, D - 1] +..., m^2, {n, s}] encodes a generic Cartesian propagator denominator 1/[(q1+...)^2 + q1.p1 + ... + m^2 + s*I eta]^n. CartesianPropagatorDenominator is an internal object. To enter such propagators in FeynCalc you should use CFAD..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*CFAD, FeynAmpDenominator.*)



(* ::Subsection:: *)
(* Examples *)



FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p,D-1],0,m^2,{1,-1}]]

FeynAmpDenominator[CartesianPropagatorDenominator[0,CartesianPair[CartesianMomentum[p,D-1],CartesianMomentum[q,D-1]],m^2,{1,-1}]]
