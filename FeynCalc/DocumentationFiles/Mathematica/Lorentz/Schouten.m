(* ::Package:: *)

 


(* ::Section:: *)
(*Schouten*)


(* ::Text:: *)
(*`Schouten[exp]` attempts to automatically remove spurious terms in `exp` by applying the Schouten's identity.*)


(* ::Text:: *)
(*`Schouten` applies the identity for $4$-vectors on at most $42$ terms in a sum. If it should operate on a larger expression you can give a second argument, e.g. `Schouten[expr, 4711]` which will work on sums with less than $4711$ terms.*)


(* ::Text:: *)
(*`Schouten` is also an option of `Contract` and `DiracTrace`. It may be set to an integer indicating the maximum number of terms onto which the function `Schouten` will be applied.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [DiracTrace](DiracTrace.md), [FCSchoutenBruteForce](FCSchoutenBruteForce.md).*)


(* ::Subsection:: *)
(*Examples*)


((LC[\[Mu],\[Nu],\[Rho],\[Sigma]] FV[p,\[Tau]]+LC[\[Nu],\[Rho],\[Sigma],\[Tau]] FV[p,\[Mu]]+LC[\[Rho],\[Sigma],\[Tau],\[Mu]] FV[p,\[Nu]]+
LC[\[Sigma],\[Tau],\[Mu],\[Nu]] FV[p,\[Rho]]+LC[\[Tau],\[Mu],\[Nu],\[Rho]] FV[p,\[Sigma]]))
Schouten[%]
