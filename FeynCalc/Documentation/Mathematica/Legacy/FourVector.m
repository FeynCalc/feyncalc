(* ::Package:: *)

 


(* ::Section:: *)
(*FourVector*)


(* ::Text:: *)
(*`FourVector[p, mu]` is the $4$-dimensional vector `p` with Lorentz index `mu`.*)


(* ::Text:: *)
(*A vector with space-time Dimension $D$ is obtained by supplying the option `Dimension -> D`.*)


(* ::Text:: *)
(*The shortcut `FourVector` is deprecated, please use `FV` instead!*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FV](FV.md), [FCI](FCI.md).*)


(* ::Subsection:: *)
(*Examples*)


FourVector[p,\[Mu]]


FourVector[p-q,\[Mu]]


StandardForm[FourVector[p,\[Mu]]]


StandardForm[FourVector[p,\[Mu],Dimension->D]]


(* ::Text:: *)
(*`FourVector` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use `FV`.*)


FV[p,\[Mu]]


FVD[p,\[Mu]]


FCI[FV[p,\[Mu]]]===FourVector[p,\[Mu]]


FCI[FVD[p,\[Mu]]]===FourVector[p,\[Mu],Dimension->D]
