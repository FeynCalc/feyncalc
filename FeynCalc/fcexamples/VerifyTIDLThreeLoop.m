(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: VerifyTIDLThreeLoop												*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:	Verify 3-loop tensor decomposition formulas stored in TIDL
				using Tdec. Warning: The computations can take quite
				some time...												*)

(* ------------------------------------------------------------------------ *)


<<HighEnergyPhysics`FeynCalc`;

On[Assert];

checkFormula[ex_] :=
	Assert[Simplify[Tdec[Sequence @@ ex, UseTIDL -> False, List -> False,
			FeynCalcExternal -> False] - TIDL[Sequence @@ ex]] === 0];

Print["Checking three-loop tensor decompositions"];


Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 d^D q3 q1^mu q2^nu q3^rho f(q1,q2,p1)"];
ex = {{{q1, mu},{q2, nu},{q3, rho}}, {p1}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["d^D q1 d^D q2 d^D q3 q1^mu q2^nu q3^rho f(q1,q2,q3,p1,p2)"];
ex = {{{q1, mu},{q2, nu},{q3, rho}}, {p1,p2}};
checkFormula[ex]
]
