(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopFindMomentumShifts*)


(* ::Text:: *)
(*`FCLoopFindMomentumShifts[source, target, {p1, p2, ...}]` finds loop momentum shifts that bring loop integrals or topologies in the list `source` to the form specified in target. The integrals/topologies in `intFrom` and `intTo` are assumed to be equivalent and their denominators must be properly ordered via `FCLoopToPakForm`. Furthermore, `target` must be provided as a list of `FeynAmpDenominator` objects, while `intFrom` is a list of such lists.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakOrder](FCLoopPakOrder).*)


(* ::Subsection:: *)
(*Examples*)


source={{FAD[p4], FAD[p1], FAD[p1 - p3 - p4],
FAD[{p1 - p4, m1}], FAD[{p3, m1}], FAD[p3 + q1],
FAD[p1 + q1]}}


target={FAD[p4], FAD[p1 + p4 + q1], FAD[p1 - p3 + q1],
FAD[{p1 + q1, m1}], FAD[{p3, m1}], FAD[p3 + q1],
FAD[p1 + p4 + 2 q1]}


FCLoopFindMomentumShifts[source, target, {p1, p3, p4}]


source={FCTopology[
fctopology3, {SFAD[{{p1, 0}, {0, 1}, 1}],
	SFAD[{{p3, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p2 + p3, 0}, {0, 1}, 1}],
	SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p2, 0}, {0, 1}, 1}]}],
FCTopology[
fctopology4, {SFAD[{{p2 + p3, 0}, {0, 1}, 1}],
	SFAD[{{p3, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}],
	SFAD[{{p1 + p3, 0}, {0, 1}, 1}]}]}


target=FCTopology[
fctopology1, {SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}],
SFAD[{{p3, 0}, {0, 1}, 1}],
SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}],
SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}],
SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}],
SFAD[{{p2 + p3, 0}, {0, 1}, 1}],
SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}]}]


FCLoopFindMomentumShifts[source,target, {p1, p2, p3}]
