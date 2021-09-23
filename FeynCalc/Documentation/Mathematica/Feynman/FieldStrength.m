(* ::Package:: *)

 


(* ::Section:: *)
(*FieldStrength*)


(* ::Text:: *)
(*`FieldStrength[\[Mu], \[Nu], a]` is the field strength tensor $\partial _{\mu } A_{\nu }^a - \partial _{\nu } A_{\mu }^a + g_s A_{\mu }^b A_{\nu }^c f^{abc}$.*)


(* ::Text:: *)
(*`FieldStrength[\[Mu], \[Nu]]` is the field strength tensor $(\partial _{\mu } A_{\nu}- \partial_{\nu } A_{\mu})$.*)


(* ::Text:: *)
(*The name of the field ($A$) and the coupling constant ($g$) can be set through the options or by additional arguments. The first two indices are interpreted as type `LorentzIndex`, except `OPEDelta`, which is converted to `Momentum[OPEDelta]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


FieldStrength[\[Mu],\[Nu]]


FieldStrength[\[Mu],\[Nu],a]


FieldStrength[\[Mu],\[Nu],Explicit->True]


FieldStrength[\[Mu],\[Nu],a,Explicit->True]


FieldStrength[\[Mu],\[Nu],a,CouplingConstant->-SMP["g_s"],Explicit->True]
