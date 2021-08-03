(* ::Package:: *)

 


(* ::Section:: *)
(*DeltaFunctionPrime*)


(* ::Text:: *)
(*`DeltaFunctionPrime[1 - x]` is the derivative of the Dirac delta-function $\delta (x)$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Convolute](Convolute), [DeltaFunction](DeltaFunction), [DeltaFunctionDoublePrime](DeltaFunctionDoublePrime), [Integrate2](Integrate2), [SimplifyDeltaFunction](SimplifyDeltaFunction).*)


(* ::Subsection:: *)
(*Examples*)


DeltaFunctionPrime[1-x]


Integrate2[DeltaFunctionPrime[1-x] f[x],{x,0,1}]


Integrate2[DeltaFunctionPrime[1-x] x^2,{x,0,1}]
