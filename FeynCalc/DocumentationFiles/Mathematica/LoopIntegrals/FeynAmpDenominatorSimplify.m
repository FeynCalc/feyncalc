(* ::Package:: *)

 


(* ::Section:: *)
(*FeynAmpDenominatorSimplify*)


(* ::Text:: *)
(*`FeynAmpDenominatorSimplify[exp]` tries to simplify each `PropagatorDenominator` in a canonical way. `FeynAmpDenominatorSimplify[exp, q1]` simplifies all `FeynAmpDenominator`s in `exp` in a canonical way, including momentum shifts. Scaleless integrals are discarded.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [TID](TID.md).*)


(* ::Subsection:: *)
(*Examples*)


FDS


(* ::Text:: *)
(*The cornerstone of dimensional regularization is that $\int d^n k f(k)/k^4 = 0$*)


FeynAmpDenominatorSimplify[f[k] FAD[k,k],k]


(* ::Text:: *)
(*This brings some loop integrals into a standard form.*)


FeynAmpDenominatorSimplify[FAD[k-Subscript[p, 1],k-Subscript[p, 2]],k]


FeynAmpDenominatorSimplify[FAD[k,k,k-q],k]


FeynAmpDenominatorSimplify[f[k]FAD[k,k-q,k-q],k]


FeynAmpDenominatorSimplify[FAD[k-Subscript[p, 1],k-Subscript[p, 2]] SPD[k,k],k]
ApartFF[%,{k}]
TID[%,k]//Factor2


FDS[FAD[k-p1,k-p2]SPD[k,OPEDelta]^2,k]
