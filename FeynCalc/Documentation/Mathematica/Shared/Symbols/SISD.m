(* ::Package:: *)

 


(* ::Section:: *)
(*SISD*)


(* ::Text:: *)
(*`SISD[p]` can be used as input for $D-1$-dimensional $\sigma^{\mu } p_{\mu }$ with $D$-dimensional Lorentz vector $p$ and is transformed into `PauliSigma[Momentum[p,D],D-1]` by `FeynCalcInternal`.*)



(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [SIS](SIS.md).*)


(* ::Subsection:: *)
(*Examples*)


SISD[p]


SISD[p]//FCI//StandardForm


SISD[p,q,r,s]
%//StandardForm


SISD[q] . (SISD[p]+m) . SISD[q]
