(* ::Package:: *)

 


(* ::Section:: *)
(*FCVariable*)


(* ::Text:: *)
(*`FCVariable`  is a data type. E.g. `DataType[z, FCVariable] = True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [DataType](DataType.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*If we want to introduce constants `c1` and `c2`, the naive way doesn't lead to the desired result*)


SPD[c1 p1+c2 p2,q]//ExpandScalarProduct


(* ::Text:: *)
(*The solution is to declare `c1` and `c2` as `FCVariable` so that FeynCalc can distinguish them from the 4-momenta*)


DataType[c1,FCVariable]=True;
DataType[c2,FCVariable]=True;
SPD[c1 p1+c2 p2,q]//ExpandScalarProduct


(* ::Text:: *)
(*This works also for propagator denominators and matrices*)


FCI[SFAD[{q+c1 p1,m}]]
%//StandardForm


GAD[\[Mu]] . (GSD[c1 p]+m) . GAD[\[Nu]]//FCI
%//StandardForm


CSI[i] . CSIS[c1 p] . CSI[j]//FCI
%//StandardForm


(* ::Text:: *)
(*To undo the declarations use*)


DataType[c1,FCVariable]=False
DataType[c2,FCVariable]=False
