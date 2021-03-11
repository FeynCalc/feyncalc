(* ::Package:: *)

(* :Title: VerifyTIDLFourLoop												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Verify 4-loop tensor decomposition formulas stored in TIDL
				using Tdec. Warning: The computations can take quite
				some time...												*)

(* ------------------------------------------------------------------------ *)

$FeynCalcStartupMessages = False;
<<FeynCalc`;

On[Assert];

checkFormula[ex_] :=
	Assert[Simplify[Tdec[Sequence @@ ex, UseTIDL -> False, List -> False,
			FeynCalcExternal -> False] - TIDL[Sequence @@ ex]] === 0];

Print["Checking four-loop tensor decompositions"];


Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 d^D q3 q1^mu q2^nu q3^rho q4^sigma f(q1,q2,q3,q4)"];
ex = {{{q1, mu},{q2, nu},{q3, rho},{q4, sigma}}, {}};
checkFormula[ex]
]



