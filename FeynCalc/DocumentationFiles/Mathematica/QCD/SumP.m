 
(* ::Section:: *)
(* SumP *)
(* ::Text:: *)
(*SumP[k, m] is $2^{k-1}\sum _{i=1}^{2m}\left(1+(-1)^i\right)/i^k$.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*SumS, SumT.*)



(* ::Subsection:: *)
(* Examples *)



SumP[1,m-1]

SumP[2,m-1]

SumP[1,m]

SumP[1,4]

\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(8\)]\(\((1 + \((\(-1\))\)^i)\)/i\)\)

Explicit[SumP[1,n/2]]

%/.n->8
