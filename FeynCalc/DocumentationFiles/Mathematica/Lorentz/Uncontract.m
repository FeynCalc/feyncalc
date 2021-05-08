 
(* ::Section:: *)
(* Uncontract *)
(* ::Text:: *)
(*Uncontract[exp, q1, q2, ...] uncontracts Eps and DiracGamma. Uncontract[exp, q1, q2, Pair -> {p}] uncontracts also p.q1 and p.q2; the option Pair -> All uncontracts all momenta except OPEDelta..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Contract.*)



(* ::Subsection:: *)
(* Examples *)



 LC[\[Mu],\[Nu]][p,q]

Uncontract[%, p]

GS[p]

Uncontract[%, p]

Uncontract[LC[\[Mu],\[Nu]][p,q], p,q]


(* ::Text:: *)
(*By default scalar products are not uncontracted.*)


Uncontract[SP[p,q], q]


(* ::Text:: *)
(*With the option Pair\[Rule]All they are \[OpenCurlyDoubleQuote]uncontracted \[CloseCurlyDoubleQuote].*)


Uncontract[SP[p,q],q,Pair->All]

Uncontract[SP[p,q]^2,q,Pair->All]
