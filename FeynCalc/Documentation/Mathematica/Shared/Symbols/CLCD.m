(* ::Package:: *)

 


(* ::Section:: *)
(*CLCD*)


(* ::Text:: *)
(*`CLCD[m, n, r]`  evaluates to `Eps[CartesianIndex[m, D-1], CartesianIndex[n, D-1], CartesianIndex[r,D-1]]` applying `FeynCalcInternal`.*)


(* ::Text:: *)
(*`CLC[m,...][p, ...]` evaluates to `Eps[CartesianIndex[m, D-1], ..., CartesianMomentum[p, D-1], ...]` applying `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [LCD](LCD.md), [Eps](Eps.md).*)


(* ::Subsection:: *)
(*Examples*)


CLCD[i,j,k]
%//FCI//StandardForm


CLCD[i,j][p]
%//FCI//StandardForm


CLCD[i,j][p]CLCD[i,j][q]//Contract//Factor2
