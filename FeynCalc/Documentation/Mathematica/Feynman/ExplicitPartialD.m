(* ::Package:: *)

 


(* ::Section:: *)
(*ExplicitPartialD*)


(* ::Text:: *)
(*`ExplicitPartialD[exp]` inserts the definitions for `LeftRightPartialD`, `LeftRightPartialD2`, `LeftRightNablaD`, `LeftRightNablaD2`, `LeftNablaD` and `RightNablaD`*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [LeftRightPartialD2](LeftRightPartialD2.md), [LeftRightNablaD](LeftRightNablaD.md), [LeftRightNablaD2](LeftRightNablaD2.md), [LeftNablaD](LeftNablaD.md), [RightNablaD](RightNablaD.md).*)


(* ::Subsection:: *)
(*Examples*)


LeftRightPartialD[\[Mu]]

ExplicitPartialD[%]


LeftRightPartialD2[\[Mu]]

ExplicitPartialD[%]


LeftRightPartialD[OPEDelta]

ExplicitPartialD[%]


16 LeftRightPartialD[OPEDelta]^4

ExplicitPartialD[%]


(* ::Text:: *)
(*Notice that by definition $\nabla^i = \partial_i = - \partial^i$, where the last equality depends on the metric signature.*)


LeftNablaD[i]

ExplicitPartialD[%]


RightNablaD[i]

ExplicitPartialD[%]


LeftRightNablaD[i]

ExplicitPartialD[%]


LeftRightNablaD2[\[Mu]]

ExplicitPartialD[%]
