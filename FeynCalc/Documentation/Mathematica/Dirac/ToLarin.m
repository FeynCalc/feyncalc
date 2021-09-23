(* ::Package:: *)

 


(* ::Section:: *)
(*ToLarin*)


(* ::Text:: *)
(*ToLarin[exp]  substitutes $\gamma^{\mu} \gamma^5$ with $-\frac{I}{6}\varepsilon^{\mu \nu \lambda \sigma } \gamma^{\nu } \gamma^{\lambda} \gamma^{\sigma }$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [DiracGamma](DiracGamma.md).*)


(* ::Subsection:: *)
(*Examples*)


GAD[\[Mu],\[Nu]] . GA[5]
ToLarin[%]
