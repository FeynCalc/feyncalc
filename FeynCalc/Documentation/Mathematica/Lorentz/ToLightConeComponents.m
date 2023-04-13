(* ::Package:: *)

 


(* ::Section:: *)
(*ToLightConeComponents*)


(* ::Text:: *)
(*`ToLightConeComponents[expr, n, nb]` rewrites all Dirac matrices, scalar products, 4-vectors and metric tensors in terms of their component along the lightcone directions `n` and `nb`*)


(* ::Text:: *)
(*Using the option `NotMomentum` one can specify that quantities containing the listed 4-momenta should be left untouched.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [LightConePerpendicularComponent](LightConePerpendicularComponent.md).*)


(* ::Subsection:: *)
(*Examples*)


ToLightConeComponents[SP[a,b],n,nb]


ToLightConeComponents[FV[p,\[Mu]],n,nb]


ToLightConeComponents[GA[\[Mu]],n,nb]


ToLightConeComponents[GS[p],n,nb]
