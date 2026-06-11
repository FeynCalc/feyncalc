(* ::Package:: *)

 


(* ::Section:: *)
(*CovariantD*)


(* ::Text:: *)
(*`CovariantD[mu]` is a generic covariant derivative with Lorentz index $\mu$.*)


(* ::Text:: *)
(*`CovariantD[x, mu]` is a generic covariant derivative with respect to $x^{\mu }$.*)


(* ::Text:: *)
(*`CovariantD[mu, a, b]` is a covariant derivative for a bosonic field that acts on `QuantumField[f, {}, {a, b}]`, where `f` is some field name and `a` and `b` are two $SU(N)$ indices in the adjoint representation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)


(* ::Subsection:: *)
(*Examples*)


CovariantD[\[Mu]]


CovariantD[\[Mu],a,b]


CovariantD[\[Mu],Explicit->True]


CovariantD[\[Mu],a,b,Explicit->True]//StandardForm
