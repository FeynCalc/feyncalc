(* ::Package:: *)

 


(* ::Section:: *)
(*DiracMatrix*)


(* ::Text:: *)
(*`DiracMatrix[mu]` denotes a Dirac gamma matrix with Lorentz index $\mu$.*)


(* ::Text:: *)
(*`DiracMatrix[mu , nu , ...]` is a product of $\gamma$ matrices with Lorentz indices `mu , nu , ...`*)


(* ::Text:: *)
(*`DiracMatrix[5]` is $\gamma ^5$.*)


(* ::Text:: *)
(*`DiracMatrix[6]` is $(1 + \gamma^5)/2$.*)


(* ::Text:: *)
(*`DiracMatrix[7]` is $(1 - \gamma^5)/2$.*)


(* ::Text:: *)
(*The shortcut `DiracMatrix` is deprecated, please use `GA` instead!*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GA](GA.md), [FCI](FCI.md).*)


(* ::Subsection:: *)
(*Examples*)


DiracMatrix[\[Mu]]


(* ::Text:: *)
(*This is how to enter the non-commutative product of two. The Mathematica Dot "." is used as non-commutative multiplication operator.*)


DiracMatrix[\[Mu]] . DiracMatrix[\[Nu]]


DiracMatrix[\[Alpha]]//StandardForm


(* ::Text:: *)
(*`DiracMatrix` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use `GA`.*)


GA[\[Mu]]


GAD[\[Mu]]


FCI[GA[\[Mu]]]===DiracMatrix[\[Mu]]


FCI[GAD[\[Mu]]]===DiracMatrix[\[Mu],Dimension->D]
