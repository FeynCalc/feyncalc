(* ::Package:: *)

 


(* ::Section:: *)
(*FCMellinJoin*)


(* ::Text:: *)
(*`FCMellinJoin[int, {q1, q2, ...}, {prop1, prop2, ...}]` applies the standard formula for splitting propagators `prop1, prop2, ...` into summands by introducing integrations along a contour in the complex space.*)


(* ::Text:: *)
(*The main purpose of this routine is to convert massive propagators into massless ones when using Mellin-Barnes integration techniques.*)


(* ::Text:: *)
(*The output consists of a list containing two elements, the first one being the prefactor and the second one the product of remaining propagators. The second element (or, alternatively, the product of both elements) can be then further processed using `FCFeynmanParametrize`. Setting the option `List` to `False` will return a product instead of a list.*)


(* ::Text:: *)
(*The option `FCSplit` can be used to split a propagators in more than 2 terms as it is done by default.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md).*)


(* ::Subsection:: *)
(*Examples*)


FCMellinJoin[FAD[q,{-p1+q,m},{-p1+q,m},{-p2+q,m}],{q},{SFAD[{q-p1,m^2}],
SFAD[{q-p2,m^2}]},Names->z]


FCMellinJoin[FAD[q,{-p1+q,m},{-p1+q,m},{-p2+q,m}],{q},{SFAD[{q-p1,m^2}],
SFAD[{q-p2,m^2}]},Names->z,FCSplit->{{q,m,p1},{q,m,p2}}]


FCMellinJoin[SFAD[{k,m^2,nu1},{p-k,0,nu2}],{k},{SFAD[{k,m^2}]},
FCLoopSwitchEtaSign->-1,Names->z]
