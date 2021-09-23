(* ::Package:: *)

 


(* ::Section:: *)
(*DiracGamma*)


(* ::Text:: *)
(*`DiracGamma[x, dim]` is the head of all Dirac matrices and slashes (in the internal representation). Use `GA`, `GAD`, `GS` or `GSD` for manual (short) input.*)


(* ::Text:: *)
(*`DiracGamma[x, 4]` simplifies to `DiracGamma[x]`.*)


(* ::Text:: *)
(*`DiracGamma[5]`  is $\gamma ^5$.*)


(* ::Text:: *)
(*`DiracGamma[6]` is $(1+\gamma ^5)/2$.*)


(* ::Text:: *)
(*`DiracGamma[7]` is $(1-\gamma ^5)/2$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGammaExpand](DiracGammaExpand.md), [GA](GA.md), [DiracSimplify](DiracSimplify.md), [GS](GS.md), [DiracTrick](DiracTrick.md).*)


(* ::Subsection:: *)
(*Examples*)


DiracGamma[5]


DiracGamma[LorentzIndex[\[Alpha]]]


(* ::Text:: *)
(*A Dirac-slash, i.e., $\gamma ^{\mu }q_{\mu}$, is displayed as $\gamma \cdot q$.*)


DiracGamma[Momentum[q]] 


DiracGamma[Momentum[q]] . DiracGamma[Momentum[p-q]]


DiracGamma[Momentum[q,D],D] 


GS[p-q] . GS[p]
DiracGammaExpand[%]


ex=GAD[\[Mu]] . GSD[p-q] . GSD[q] . GAD[\[Mu]]


DiracTrick[ex]


DiracSimplify[ex]


(* ::Text:: *)
(*`DiracGamma` may also carry Cartesian indices or appear contracted with Cartesian momenta.*)


DiracGamma[CartesianIndex[i]]


DiracGamma[CartesianIndex[i,D-1],D]


DiracGamma[CartesianMomentum[p]]


DiracGamma[CartesianMomentum[p,D-1],D]


(* ::Text:: *)
(*Temporal indices are represented using `ExplicitLorentzIndex[0]`*)


DiracGamma[ExplicitLorentzIndex[0]]
