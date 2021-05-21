(* ::Package:: *)

 


(* ::Section:: *)
(* ExplicitPartialD *)


(* ::Text:: *)
(*`ExplicitPartialD[exp]` inserts the definitions for `LeftRightPartialD` and `LeftRightPartialD2`.*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*ExpandPartialD, LeftRightPartialD, LeftRightPartialD2.*)


(* ::Subsection:: *)
(* Examples *)


LeftRightPartialD[\[Mu]]
ExplicitPartialD[%]


LeftRightPartialD2[\[Mu]]
ExplicitPartialD[%]


LeftRightPartialD[OPEDelta]
ExplicitPartialD[%]


16 LeftRightPartialD[OPEDelta]^4
ExplicitPartialD[%]
