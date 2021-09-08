(* ::Package:: *)

 


(* ::Section:: *)
(*SUNIndex*)


(* ::Text:: *)
(*`SUNIndex[a]` is an $SU(N)$ index in the adjoint representation. If the argument is an integer, `SUNIndex[a]` turns into `ExplicitSUNIndex[a]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitSUNIndex](ExplicitSUNIndex.md), [SUNDelta](SUNDelta.md), [SUNF](SUNF.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNIndex[i]
%//StandardForm


SUNIndex[2]
%//StandardForm


SUNDelta[i,j]//FCI//StandardForm
