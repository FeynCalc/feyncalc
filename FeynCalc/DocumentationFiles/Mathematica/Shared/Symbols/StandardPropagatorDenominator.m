 
(* ::Section:: *)
(* StandardPropagatorDenominator *)
(* ::Text:: *)
(*StandardPropagatorDenominator[Momentum[q1, D] +..., Pair[Momentum[q1, D], Momentum[p1, D] +..., m^2, {n, s}] encodes a generic Lorentzian propagator denominator 1/[(q1+...)^2 + q1.p1 + ... + m^2 + s*I eta]^n. This allows to accomodate for standard propagators of the type 1/(p^2-m^2) but also for propagators encountered in manifestly Lorentz covariant effective field theories such as HQET or SCET. StandardPropagatorDenominator is an internal object. To enter such propagators in FeynCalc you should use SFAD..*)


(* ::Subsection:: *)
(* Examples *)
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,-m^2,{1,1}]]

FeynAmpDenominator[StandardPropagatorDenominator[0,Pair[Momentum[p,D],Momentum[q,D]],-m^2,{1,1}]]
