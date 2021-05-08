 
(* ::Section:: *)
(* FeynAmpDenominatorSimplify *)
(* ::Text:: *)
(*FeynAmpDenominatorSimplify[exp] tries to simplify each PropagatorDenominator in a canonical way. FeynAmpDenominatorSimplify[exp, q1] simplifies all FeynAmpDenominator's in exp in a canonical way, including some translation of momenta. FeynAmpDenominatorSimplify[exp, q1, q2] additionally removes integrals with no mass scale.FDS can be used as an alias..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*TID.*)



(* ::Subsection:: *)
(* Examples *)



FDS


(* ::Text:: *)
(*The cornerstone of dimensional regularization is that $int d^nk f(k)left/k^{2m}right.= 0 .$*)


FeynAmpDenominatorSimplify[f[k] FAD[k,k],k]


(* ::Text:: *)
(*This brings $1left/left(left(k-p_1right){}^2 left(k-p_2right){}^2right)right.$ into a standard form.*)


FeynAmpDenominatorSimplify[FAD[k-Subscript[p, 1],k-Subscript[p, 2]],k]

FeynAmpDenominatorSimplify[FAD[k,k,k-q],k]

FeynAmpDenominatorSimplify[f[k]FAD[k,k-q,k-q],k]

FeynAmpDenominatorSimplify[FAD[k-Subscript[p, 1],k-Subscript[p, 2]] SPD[k,k],k]

ApartFF[%,{k}]

TID[%,k]//Factor2

FDS[FAD[k-p1,k-p2]SPD[k,OPEDelta]^2,k]
