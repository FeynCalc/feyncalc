(* ::Package:: *)

 


(* ::Section:: *)
(*Solve3*)


(* ::Text:: *)
(*`Solve3` is equivalent to `Solve`, except that it works only for linear equations (and returns just a list) and uses the "high school algorithm.*)


(* ::Text:: *)
(*Sometimes it is better than `Solve` for systems involving rational polynomials.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Solve2](Solve2.md).*)


(* ::Subsection:: *)
(*Examples*)


Solve3[{2 x==b-w/2,y-d==p},{x,y}]
