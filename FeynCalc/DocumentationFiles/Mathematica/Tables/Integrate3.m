(* ::Package:: *)

 


(* ::Section:: *)
(*Integrate3*)


(* ::Text:: *)
(*`Integrate3` contains the integral table used by `Integrate2`. Integration is performed in a distributional sense. `Integrate3` works more effectively on a sum of expressions if they are expanded or collected with respect to the integration variable. See the examples in `Integrate2`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Integrate2](Integrate2.md).*)


(* ::Subsection:: *)
(*Examples*)


Integrate3[x^OPEm Log[x],{x,0,1}]


Integrate3[(x^OPEm Log[x] Log[1-x])/(1-x),{x,0,1}]


Integrate3[a (x^OPEm Log[x] Log[1-x])/(1-x)+b (x^OPEm PolyLog[3,-x])/(1+x),{x,0,1}]


Integrate3[DeltaFunctionPrime[1-x],{x,0,1}]


Integrate3[f[x] DeltaFunctionPrime[1-x],{x,0,1}]


Integrate3[1/(1-x),{x,0,1}]
