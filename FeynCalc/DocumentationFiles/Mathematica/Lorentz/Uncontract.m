(* ::Package:: *)

 


(* ::Section:: *)
(*Uncontract*)


(* ::Text:: *)
(*`Uncontract[exp, q1, q2, ...]` uncontracts `Eps` and `DiracGamma`.*)


(* ::Text:: *)
(*`Uncontract[exp, q1, q2, Pair -> {p}]` uncontracts also $p \cdot q_1$ and $p \cdot q_2$; *)


(* ::Text:: *)
(*The option `Pair -> All` uncontracts all momenta except `OPEDelta`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Contract](Contract.md).*)


(* ::Subsection:: *)
(*Examples*)


LC[\[Mu],\[Nu]][p,q]
Uncontract[%, p]


GS[p]
Uncontract[%, p]


Uncontract[LC[\[Mu],\[Nu]][p,q], p,q]


(* ::Text:: *)
(*By default scalar products are not uncontracted.*)


Uncontract[SP[p,q], q]


(* ::Text:: *)
(*Use the option `Pair->All` to make the function take care of the scalar products as well*)


Uncontract[SP[p,q],q,Pair->All]


Uncontract[SP[p,q]^2,q,Pair->All]


(* ::Text:: *)
(*For Cartesian scalar products you need to use the option `CartesianPair->All`*)


Uncontract[CSP[p,q],q,Pair->All]


Uncontract[CSP[p,q],q,CartesianPair->All]
