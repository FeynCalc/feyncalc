(* ::Package:: *)

 


(* ::Section:: *)
(*DeltaFunction*)


(* ::Text:: *)
(*`DeltaFunction[x]` is the Dirac delta-function $\delta (x)$.*)


(* ::Text:: *)
(*Mathematica also provides a built-in function `DiracDelta` with comparable properties.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Convolute](Convolute.md), [DeltaFunctionPrime](DeltaFunctionPrime.md), [Integrate2](Integrate2.md), [SimplifyDeltaFunction](SimplifyDeltaFunction.md).*)


(* ::Subsection:: *)
(*Examples*)


DeltaFunction[1-x]


Integrate2[DeltaFunction[1-x] f[x],{x,0,1}]


Integrate2[DeltaFunction[x] f[x],{x,0,1}]


Integrate2[DeltaFunction[1-x] f[x],{x,0,1}]


Convolute[DeltaFunction[1-x],x]/.FCGV[z_]:>ToExpression[z]
