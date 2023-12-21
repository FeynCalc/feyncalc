(* ::Package:: *)

 


(* ::Section:: *)
(*FCGetScalarProducts*)


(* ::Text:: *)
(*`FCGetScalarProducts[{p1, p2, ...}]` returns all scalar products involving external momenta `p1, p2, ...` that were set using down values.*)


(* ::Text:: *)
(*Using the option `SetDimensions` one can specify the dimensions of scalar products one is interested in.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md), [Pair](Pair.md), [SP](SP.md), [SPD](SPD.md).*)


(* ::Subsection:: *)
(*Examples*)


FCClearScalarProducts[];
SP[p1]=m1^2;
SP[p1]=m2^2;
SP[p1,p2]=s;
SPD[q1]=M1^2;
SPD[q2]=M2^2;
SPD[q1,q2]=t;


FCGetScalarProducts[{p1,p2,q1,q2}]


FCGetScalarProducts[{p1,p2,q1,q2},SetDimensions->{4}]


FCGetScalarProducts[{p1,p2,q1,q2},SetDimensions->{D}]
