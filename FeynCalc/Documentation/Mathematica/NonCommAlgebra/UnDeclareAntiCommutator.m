(* ::Package:: *)

 


(* ::Section:: *)
(*UnDeclareAntiCommutator*)


(* ::Text:: *)
(*`UnDeclareAntiCommutator[a, b]` undeclares the value assigned to the anti-commutator of `a` and `b`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCCommutator](FCCommutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


FCAntiCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]],A],QuantumField[A]]=0;


QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]]

ExpandPartialD[%]


UnDeclareAntiCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]],A],QuantumField[A]];


ExpandPartialD[QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]]]
