 
(* ::Section:: *)
(*HypInt*)
(* ::Text:: *)
(*`HypInt[exp, t]` substitutes all `Hypergeometric2F1[a,b,c,z]` in `exp` with `Gamma[c]/(Gamma[b] Gamma[c-b]) Integratedx[t,0,1]  t^(b-1) (1-t)^(c-b-1) (1-t z)^(-a)`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Series2](Series2.md).*)


(* ::Subsection:: *)
(*Examples*)


Hypergeometric2F1[a,b,c,z]
HypInt[%,t]
