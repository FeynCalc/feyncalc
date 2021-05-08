 
(* ::Section:: *)
(* FCApart *)
(* ::Text:: *)
(*FCApart[expr, {q1, q2, ...}] is an internal function that partial fractions a loop integral (that depends on q1,q2,...) into integrals that contain only linearly independent propagators. The algorithm is largely based on the work and code of F. Feng (arXiv:1204.2314). FCApart is meant to be applied to single loop integrals only. If you need to perform partial fractioning on an expression that contains multiple loop integrals, use ApartFFThere is actually no reason, why one would want to apply FCApart instead of ApartFF, except for cases, where FCApart is called from a different package that interacts with FeynCalc..*)


(* ::Subsection:: *)
(* Examples *)
ApartFF
SPD[q,q]FAD[{q,m}]
FCApart[%,{q}]


SPD[q,p]SPD[q,r]FAD[{q},{q-p},{q-r}]
FCApart[%,{q}]


SPD[p,q1]SPD[p,q2]^2FAD[{q1,m},{q2,m},q1-p,q2-p,q1-q2]
FCApart[%,{q1,q2}]

