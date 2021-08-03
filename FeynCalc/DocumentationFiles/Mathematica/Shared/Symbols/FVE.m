(* ::Package:: *)

 


(* ::Section:: *)
(*FVE*)


(* ::Text:: *)
(*`FVE[p, mu]` is the $D-4$-dimensional vector $p$ with Lorentz index $\mu$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCE](FCE), [FCI](FCI), [FV](FV), [FVD](FVD), [Pair](Pair).*)


(* ::Subsection:: *)
(*Examples*)


FVE[p,\[Mu]]


FVE[p-q,\[Mu]]


FVE[p,\[Mu]]//StandardForm


FCI[FVE[p,\[Mu]]]//StandardForm


(* ::Text:: *)
(*There is no special function to expand momenta in `FVE`.*)


ExpandScalarProduct[FVE[p-q,\[Mu]]]
StandardForm[%]


Contract[FVE[p,\[Mu]]FV[q,\[Mu]]]
