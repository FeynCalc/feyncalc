(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopBasisFindCompletion*)


(* ::Text:: *)
(*`FCLoopBasisFindCompletion[int, {q1, q2, ...}]` determines propagators that need to be included in the loop integral `int` (that depends on the loop momenta `q1`, `q2`, ...), to ensure that the propagators of `int` form a basis.*)


(* ::Text:: *)
(*For integrals with propagators that do not form a basis, such a completion must be found prior to processing those integrals with tools that do Integration-By-Parts (IBP) reduction (e.g. FIRE, KIRA or LiteRed). Furthermore, `int` may not contain linearly dependent propagators.*)


(* ::Text:: *)
(*The input can also consist of an `FCTopology` object or a list thereof.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopBasisIncompleteQ](FCLoopBasisIncompleteQ.md).*)


(* ::Subsection:: *)
(*Examples*)


FAD[q,{q-p+l,m}]
FCLoopBasisFindCompletion[%,{q}]


FAD[{q1,m1},{q2,m2}]
FCLoopBasisFindCompletion[%,{q1,q2}]


FAD[q1+p,q2-k] SPD[q1,q2]
FCLoopBasisFindCompletion[%,{q1,q2},Method->{FAD[{q2+k,m}],FAD[{q1-p,m}],SPD[p,q2],SPD[k,q1]}]


(* ::Text:: *)
(*Cartesian integrals are also supported.*)


CFAD[q1,q2,{q1-l1,m1},{q2-l2,m2}]
FCLoopBasisFindCompletion[%,{q1,q2}]


(* ::Text:: *)
(*Extending `FCTopology` objects*)


FCLoopBasisFindCompletion[FCTopology[topo,{FAD[p1],FAD[p2],FAD[p1-q],FAD[p2-q]},{p1,p2},{q},{},{}]]


FCLoopBasisFindCompletion[{
FCTopology[topo1,{FAD[p1],FAD[p2],FAD[p1-q],FAD[p2-q]},{p1,p2},{q},{},{}],
FCTopology[topo2,{FAD[p1],FAD[p2],FAD[p1-q],FAD[p2-p1]},{p1,p2},{q},{},{}]
}]


(* ::Text:: *)
(*The function pays attention to the $i \eta$ signs in the propagators*)


FCLoopBasisFindCompletion[{FCTopology[
asyR1prop2Ltopo13011X11011NAux1, {SFAD[{{I*p1, 0}, {0, {-1}}, 1}],
	SFAD[{{(-I)*p3, 0}, {-mb^2, -1}, 1}],
	SFAD[{{I*(p1 + p3), 0}, {-mb^2, -1}, 1}],
	SFAD[{{I*(p1 - q), 0}, {-mb^2, -1}, 1}]}, {p1,
	p3}, {q}, {SPD[q, q] -> mb^2}, {}]}, FCE -> True]
