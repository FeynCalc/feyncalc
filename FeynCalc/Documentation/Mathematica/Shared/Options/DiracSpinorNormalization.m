(* ::Package:: *)

 


(* ::Section:: *)
(*DiracSpinorNormalization*)


(* ::Text:: *)
(*`DiracSpinorNormalization` is an option for `SpinorChainEvaluate`, `DiracSimplify` and other functions. It specifies the normalization of the spinor inner products $\bar{u}(p) u(p)$ and $\bar{v}(p) v(p)$. Following values are supported: *)


(* ::Text:: *)
(*- `"Relativistic"` - this is the standard value corresponding to $\bar{u}(p) u(p) = 2 m$, $\bar{v}(p) v(p) = - 2 m$.*)


(* ::Text:: *)
(*- `"Rest"` - this sets $\bar{u}(p) u(p) = 1$, $\bar{v}(p) v(p) = - 1$.*)


(* ::Text:: *)
(*- `"Nonrelativistic"` - this sets $\bar{u}(p) u(p) = \frac{m}{p^0}$, $\bar{v}(p) v(p) = - \frac{m}{p^0}$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [SpinorChainEvaluate](SpinorChainEvaluate.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorUBar[p,m] . SpinorU[p,m]
DiracSimplify[%]


SpinorUBar[p,m] . SpinorU[p,m]
DiracSimplify[%,DiracSpinorNormalization->"Rest"]


SpinorUBar[p,m] . SpinorU[p,m]
DiracSimplify[%,DiracSpinorNormalization->"Nonrelativistic"]



