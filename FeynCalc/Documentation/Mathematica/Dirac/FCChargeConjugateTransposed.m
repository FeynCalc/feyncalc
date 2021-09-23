(* ::Package:: *)

 


(* ::Section:: *)
(*FCChargeConjugateTransposed*)


(* ::Text:: *)
(*`FCChargeConjugateTransposed[exp]` represents the application of the charge conjugation operator to the transposed of `exp`, i.e. $C^{-1} \text{exp}^T C$. Here `exp` is understood to be a single Dirac matrix or a chain thereof. The option setting `Explicit` determines whether the explicit result is returned or whether it is left in the unevaluated form.The unevaluated form will be also maintained if the function does not know how to obtain $C^{-1} \text{exp}^T C$ from the given exp.*)


(* ::Text:: *)
(*The shortcut for `FCChargeConjugateTransposed` is `FCCCT`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SpinorChainTranspose](SpinorChainTranspose.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md).*)


(* ::Subsection:: *)
(*Examples*)


GA[\[Mu],\[Nu],\[Rho]]
FCChargeConjugateTransposed[%]


FCChargeConjugateTransposed[GA[\[Mu],\[Nu],\[Rho]],Explicit->True]


GA[5]
FCCCT[%]
%//Explicit
