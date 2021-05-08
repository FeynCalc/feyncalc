 
(* ::Section:: *)
(* Amputate *)
(* ::Text:: *)
(*Amputate[exp, q1, q2, ...] amputates Eps and DiracGamma. Amputate[exp,q1,q2,Pair->{p}] amputates also p.q1 and p.q2; Pair->All amputates all except OPEDelta..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracGamma, GA, DiracSimplify, GS, DiracTrick.*)



(* ::Subsection:: *)
(* Examples *)



GS[p].GS[q]

Amputate[%,q]
