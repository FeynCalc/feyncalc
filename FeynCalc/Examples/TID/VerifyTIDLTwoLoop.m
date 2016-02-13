(* ::Package:: *)



(* :Title: VerifyTIDLTwoLoop												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Verify 2-loop tensor decomposition formulas stored in TIDL
				using Tdec. Warning: The computations can take quite
				some time...												*)

(* ------------------------------------------------------------------------ *)

$FeynCalcStartupMessages = False;
<<FeynCalc`;

On[Assert];

checkFormula[ex_] :=
	Assert[Simplify[Tdec[Sequence @@ ex, UseTIDL -> False, List -> False,
			FeynCalcExternal -> False] - TIDL[Sequence @@ ex]] === 0];

Print["Checking two-loop tensor decompositions"];


Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q2^nu f(q1,q2)"];
ex = {{{q1, mu},{q2, nu}}, {}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q2^nu f(q1,q2,p1)"];
ex = {{{q1, mu},{q2, nu}}, {p1}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q2^nu f(q1,q2,p1,p2)"];
ex = {{{q1, mu},{q2, nu}}, {p1,p2}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q2^nu f(q1,q2,p1,p2,p3)"];
ex = {{{q1, mu},{q2, nu}}, {p1,p2,p3}};
checkFormula[ex]
]

(*	3-4 minutes on a Core i7	*)
Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q2^nu f(q1,q2,p1,p2,p3,p4)"];
ex = {{{q1, mu},{q2, nu}}, {p1,p2,p3,p4}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q1^nu q2^rho f(q1,q2,p1)"];
ex = {{{q1, mu},{q1, nu},{q2,rho}}, {p1}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q1^nu q2^rho f(q1,q2,p1,p2)"];
ex = {{{q1, mu},{q1, nu},{q2,rho}}, {p1,p2}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q1^nu q2^rho f(q1,q2,p1,p2,p3)"];
ex = {{{q1, mu},{q1, nu},{q2,rho}}, {p1,p2,p3}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q1^nu q1^rho q2^si  f(q1,q2)"];
ex = {{{q1, mu},{q1, nu},{q1,rho},{q2,si}}, {}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q1^nu q1^rho q2^si  f(q1,q2,p1)"];
ex = {{{q1, mu},{q1, nu},{q1,rho},{q2,si}}, {p1}};
checkFormula[ex]
]

(*	30 - 40 seconds on a Core i7	*)
Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 q1^mu q1^nu q1^rho q2^si  f(q1,q2,p1,p2)"];
ex = {{{q1, mu},{q1, nu},{q1,rho},{q2,si}}, {p1,p2}};
checkFormula[ex]
]
