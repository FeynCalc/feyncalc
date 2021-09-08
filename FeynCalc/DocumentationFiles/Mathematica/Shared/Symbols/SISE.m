(* ::Package:: *)

 


(* ::Section:: *)
(*SISE*)


(* ::Text:: *)
(*`SISE[p]` can be used as input for $D-4$-dimensional $\sigma ^{\mu } p_{\mu }$ with $D-4$-dimensional Lorentz vector $p$ and is transformed into `PauliSigma[Momentum[p,D-4], D-4]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SIS](SIS.md), [PauliSigma](PauliSigma.md).*)


(* ::Subsection:: *)
(*Examples*)


SISE[p]


SISE[p]//FCI//StandardForm


SISE[p,q,r,s]


SISE[p,q,r,s]//StandardForm
