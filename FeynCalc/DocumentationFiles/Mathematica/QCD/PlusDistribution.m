(* ::Package:: *)

 


(* ::Section:: *)
(*PlusDistribution*)


(* ::Text:: *)
(*`PlusDistribution[1/(1 - x)]` denotes a distribution (in the sense of the "+" prescription).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Integrate2](Integrate2.md).*)


(* ::Subsection:: *)
(*Examples*)


PlusDistribution[1/(1-x)]


PlusDistribution[Log[1-x]/(1-x)]


Integrate2[PlusDistribution[1/(1-x)], {x,0,1}]


Integrate2[PlusDistribution[Log[1-x]/(1-x)], {x,0,1}]


Integrate2[PlusDistribution[Log[1-x]^2/(1-x)], {x,0,1}]


PlusDistribution[Log[x (1-x)]/(1-x)]
