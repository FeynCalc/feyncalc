(* ::Package:: *)

 


(* ::Section:: *)
(*Solve2*)


(* ::Text:: *)
(*`Solve2` is equivalent to `Solve`, except that it works only for linear equations (and returns just a list) and accepts the options `Factoring` and `FinalSubstitutions`.*)


(* ::Text:: *)
(*`Solve2` uses the "high school algorithm" and factors intermediate results. Therefore it can be drastically more useful than `Solve`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Solve3](Solve3.md).*)


(* ::Subsection:: *)
(*Examples*)


Solve2[{2 x==b-w/2,y-d==p},{x,y}]


(* ::Text:: *)
(*If no equation sign is given the polynomials are supposed to be $0$.*)


Solve2[x+y,x]


Solve2[x+y,x,FinalSubstitutions->{y->h}]


Solve2[{2 x==b-w/2,y-d==p},{x,y},Factoring->Expand]


Solve[{2 x==b-w/2,y-d==p},{x,y}]
