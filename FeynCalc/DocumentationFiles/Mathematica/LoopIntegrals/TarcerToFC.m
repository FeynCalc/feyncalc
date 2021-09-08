(* ::Package:: *)

 


(* ::Section:: *)
(*TarcerToFC*)


(* ::Text:: *)
(*`TarcerToFC[expr, {q1, q2}]` translates loop integrals in Tarcer-notation to the FeynCalc notation.*)


(* ::Text:: *)
(*See `TFI` for details on the convention.*)


(* ::Text:: *)
(*As in the case of `ToTFI`, the \frac{1}{\pi^D} and \frac{1}{\pi^{D/2}} prefactors are implicit, i.e. `TarcerToFC` doesn't add them.*)


(* ::Text:: *)
(*To recover momenta from scalar products use the option `ScalarProduct` e.g. as in `TarcerToFC[TBI[D, pp^2, {{1, 0}, {1, 0}}], {q1, q2}, ScalarProduct -> {{pp^2, p1}}]`*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ToFI](ToFI.md).*)


(* ::Subsection:: *)
(*Examples*)


Tarcer`TFI[D,Pair[Momentum[p,D],Momentum[p,D]],{0,0,3,2,0},
{{4,0},{2,0},{1,0},{0,0},{1,0}}]


TarcerToFC[%,{q1,q2}]


a1 Tarcer`TBI[D, pp^2, {{1, 0}, {1, 0}}] + b1 Tarcer`TBI[D, mm1, {{1, 0}, {1, 0}}]


TarcerToFC[%, {q1, q2}, ScalarProduct -> {{pp^2, p1}, {mm1, p1}}, FCE -> True]
