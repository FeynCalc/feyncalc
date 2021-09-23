(* ::Package:: *)

 


(* ::Section:: *)
(*CovariantD*)


(* ::Text:: *)
(*`CovariantD[\[Mu]]` is a generic covariant derivative with Lorentz index $\mu$.*)


(* ::Text:: *)
(*`CovariantD[x, \[Mu]]` is a generic covariant derivative with respect to $x^{\mu }$.*)


(* ::Text:: *)
(*`CovariantD[\[Mu], a, b]` is a covariant derivative for a bosonic field that acts on `QuantumField[f, {}, {a, b}]`, where `f` is some field name and `a` and `b` are two $SU(N)$ indices in the adjoint representation.*)


(* ::Text:: *)
(*`CovariantD[OPEDelta, a, b]` is a short form for `CovariantD[\[Mu], a, b] FV[OPEDelta, \[Mu]]`.*)


(* ::Text:: *)
(*`CovariantD[{OPEDelta, a, b}, {n}]` yields the product of `n` operators, where `n` is an integer.   *)


(* ::Text:: *)
(*`CovariantD[OPEDelta, a, b, {m, n}]` gives the expanded form of `CovariantD[OPEDelta, a, b]^m` up to order $g^n$ for the gluon, where $n$ is an integer and $g$ the coupling constant indicated by the setting of the option `CouplingConstant`.*)


(* ::Text:: *)
(*`CovariantD[OPEDelta, {m, n}]` gives the expanded form of `CovariantD[OPEDelta]^m` up to order $g^n$ of the fermionic field. To obtain the explicit expression for a particular covariant derivative, the option `Explicit` must be set to `True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


CovariantD[\[Mu]]


CovariantD[\[Mu],a,b]


CovariantD[\[Mu],Explicit->True]


(* ::Text:: *)
(*The first argument of `CovariantD` is interpreted as type `LorentzIndex`, except for `OPEDelta`, which is type `Momentum`.*)


CovariantD[OPEDelta]


CovariantD[OPEDelta,a,b]


CovariantD[OPEDelta,a,b,Explicit->True]


CovariantD[OPEDelta,Explicit->True]


CovariantD[OPEDelta,a,b,{2}]


(* ::Text:: *)
(*This gives$m * \vec{\partial}_{\Delta}$, the partial derivative $\vec{\partial}_{\mu }$ contracted with $\Delta ^{\mu }$*)


CovariantD[OPEDelta,a,b,{OPEm,0}]


(* ::Text:: *)
(*The expansion up to first order in the coupling constant $g_s$ (the sum is the `FeynCalcOPESum`)*)


CovariantD[OPEDelta,a,b,{OPEm,1}]


(* ::Text:: *)
(*The expansion up to second order in the $g_s$*)


CovariantD[OPEDelta,a,b,{OPEm,2}]


CovariantD[OPEDelta,a,b]^OPEm


CovariantD[OPEDelta,{OPEm,2}]


CovariantD[OPEDelta,Explicit->True]//StandardForm


CovariantD[\[Mu],a,b,Explicit->True]//StandardForm
