(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopMixedIntegralQ*)


(* ::Text:: *)
(*`FCLoopMixedIntegralQ[exp]` returns `True` if the integral contains both Lorentz and Cartesian indices and momenta.*)


(* ::Subsection:: *)
(*See also*)


(* ::Subsection:: *)
(*Examples*)


FCI[FVD[p,mu] CFAD[q,q-p]]
FCLoopMixedIntegralQ[%]


FCI[FVD[p,mu] FAD[q,q-p]]
FCLoopMixedIntegralQ[%]
