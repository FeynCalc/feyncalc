(* ::Package:: *)

 


(* ::Section:: *)
(*DeltaFunctionPrime*)


(* ::Text:: *)
(*`DeltaFunctionPrime[1 - x]` is the derivative of the Dirac delta-function $\delta (x)$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Convolute](Convolute.md), [DeltaFunction](DeltaFunction.md), [DeltaFunctionDoublePrime](DeltaFunctionDoublePrime.md), [Integrate2](Integrate2.md), [SimplifyDeltaFunction](SimplifyDeltaFunction.md).*)


(* ::Subsection:: *)
(*Examples*)


DeltaFunctionPrime[1-x]


Integrate2[DeltaFunctionPrime[1-x] f[x],{x,0,1}]


Integrate2[DeltaFunctionPrime[1-x] x^2,{x,0,1}]
