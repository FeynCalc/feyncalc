(* ::Package:: *)

(* ::Section:: *)
(*UnDeclareCommutator*)


(* ::Text:: *)
(*`UnDeclareCommutator[a, b]` undeclares the value assigned to the commutator of `a` and `b`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Commutator](Commutator), [CommutatorExplicit](CommutatorExplicit), [DeclareNonCommutative](DeclareNonCommutative), [DotSimplify](DotSimplify).*)


(* ::Subsection:: *)
(*Examples*)


Commutator[QuantumField[FCPartialD[LorentzIndex[xxx_]],A],QuantumField[A]]=0;


QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]] . QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]]
ExpandPartialD[%]


UnDeclareCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]],A],QuantumField[A]];


QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]] . QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]]
ExpandPartialD[%]



