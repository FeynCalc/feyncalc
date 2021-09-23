(* ::Package:: *)

 


(* ::Section:: *)
(*QuantumField*)


(* ::Text:: *)
(*`QuantumField` is the head of quantized fields and their derivatives.*)


(* ::Text:: *)
(*`QuantumField[par, ftype, {lorind}, {sunind}]` denotes a quantum field of type `ftype` with (possible) Lorentz-indices `lorind` and $SU(N)$ indices `sunind`. The optional first argument `par` denotes a partial derivative acting on the field.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynRule](FeynRule.md), [FCPartialD](FCPartialD.md), [ExpandPartialD](ExpandPartialD.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This denotes a scalar field.*)


QuantumField[S]


(* ::Text:: *)
(*Quark fields*)


QuantumField[AntiQuarkField]


QuantumField[QuarkField]


(* ::Text:: *)
(*This is a field with a Lorentz index.*)


QuantumField[B,{\[Mu]}]


(* ::Text:: *)
(*Color indices should be put after the Lorentz ones.*)


QuantumField[GaugeField,{\[Mu]},{a}]
%//StandardForm


(* ::Text:: *)
(*$A_{\Delta}^a$ is a short form for $\Delta ^{mu } A_{mu }^a$ *)


QuantumField[A,{OPEDelta},{a}]


(* ::Text:: *)
(*The first list of indices is usually interpreted as type `LorentzIndex`, except for `OPEDelta`, which gets converted to type `Momentum`.*)


QuantumField[A,{OPEDelta},{a}]//StandardForm


(* ::Text:: *)
(*Derivatives of fields are denoted as follows.*)


QuantumField[FCPartialD[\[Mu]],A,{\[Mu]}]


QuantumField[FCPartialD[OPEDelta],S]


QuantumField[FCPartialD[OPEDelta],A,{OPEDelta},{a}]


QuantumField[FCPartialD[OPEDelta]^OPEm,A,{OPEDelta},{a}]


QuantumField[QuantumField[A]] === QuantumField[A]
