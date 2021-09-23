(* ::Package:: *)

 


(* ::Section:: *)
(*GSE*)


(* ::Text:: *)
(*`GSE[p]` can be used as input for a $D-4$-dimensional $\gamma \cdot p = \gamma^\mu p_\mu$ and is transformed into `DiracGamma[Momentum[p,D-4],D-4]` by `FeynCalcInternal` (`FCI`). `GSE[p,q, ...]` is a short form for `GSE[p].GSE[q]. ...`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GA](GA.md), [GAD](GAD.md), [GSD](GSD.md).*)


(* ::Subsection:: *)
(*Examples*)


GSE[p]
GSE[p]//FCI//StandardForm



GSE[p,q,r,s]
GSE[p,q,r,s]//StandardForm


GSE[q] . (GSE[p]+m) . GSE[q]


(* ::Text:: *)
(*In order to use Dirac algebra with $D-4$ dimensional objects you need to activate the t'Hooft-Veltman-Breitenlohner-Maison scheme first*)


FCSetDiracGammaScheme["NDR"];
DiracSimplify[GSE[q] . GS[q] . GSE[q]]


FCSetDiracGammaScheme["BMHV"];
DiracSimplify[GSE[q] . GS[q] . GSE[q]]


FCSetDiracGammaScheme["NDR"];
