 
(* ::Section:: *)
(* FCLoopNonIntegerPropagatorsFreeQ *)
(* ::Text:: *)
(*FCLoopNonIntegerPropagatorsFreeQ[exp] checks if the integral contains propagators raised to noninteger (i.e. fractional or symbolic) powers.*)


(* ::Subsection:: *)
(* Examples *)
FCI@CFAD[{q+p,m^2}]

FCLoopNonIntegerPropagatorPowersFreeQ[%]

FCI@CFAD[{q+p,m^2,1/2}]

FCLoopNonIntegerPropagatorPowersFreeQ[%]
