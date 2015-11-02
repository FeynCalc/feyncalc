(* ::Package:: *)



(* :Title: VerifyTIDLOneLoop												*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:	Verify 1-loop tensor decomposition formulas stored in TIDL
				using Tdec. Warning: The computations can take quite
				some time...												*)

(* ------------------------------------------------------------------------ *)

$FeynCalcStartupMessages = False;
<<FeynCalc`;

On[Assert];

checkFormula[ex_] :=
	Assert[Simplify[Tdec[Sequence @@ ex, UseTIDL -> False, List -> False,
			FeynCalcExternal -> False] - TIDL[Sequence @@ ex]] === 0];

Print["Checking one-loop tensor decompositions"];


Print @ AbsoluteTiming[
Print ["A^{mu}:"];
ex = {{{q, mu}}, {}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["A^{mu,nu}:"];
ex = {{{q, mu}, {q, nu}}, {}};
checkFormula[ex]
]


Print @ AbsoluteTiming[
Print ["A^{mu,nu,rho}:"];
ex = {{{q, mu}, {q, nu}, {q, rho}}, {}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["A^{mu,nu,rho,si}:"];
ex = {{{q, mu}, {q, nu}, {q, rho}, {q, si}}, {}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["A^{mu,nu,rho,si,tau,kappa}:"];
ex = {{{q, mu}, {q, nu}, {q, rho}, {q, si}, {q, tau}, {q, kappa}}, {}};
checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["B^{mu}:"];
ex = {{{q, mu}}, {p}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["B^{mu,nu}:"];
ex = {{{q, mu},{q, nu}}, {p}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["B^{mu,nu,rho}:"];
ex = {{{q, mu},{q, nu},{q, rho}}, {p}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["B^{mu,nu,rho,sigma}:"];
ex = {{{q, mu},{q, nu},{q, rho},{q, si}}, {p}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["B^{mu,nu,rho,sigma,tau}:"];
ex = {{{q, mu},{q, nu},{q, rho},{q, si},{q, tau}}, {p}}; checkFormula[ex]
]

(*	roughly 9 seconds on a Core i7	*)
Print @ AbsoluteTiming[
Print ["B^{mu,nu,rho,sigma,tau,kappa}:"];
ex = {{{q, mu},{q, nu},{q, rho},{q, si},{q, tau},{q, kappa}}, {p}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["C^{mu}:"];
ex = {{{q, mu}}, {p1,p2}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["C^{mu,nu}:"];
ex = {{{q, mu},{q, nu}}, {p1,p2}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["C^{mu,nu,rho}:"];
ex = {{{q, mu},{q, nu},{q, rho}}, {p1,p2}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["C^{mu,nu,rho,sigma}:"];
ex = {{{q, mu},{q, nu},{q, rho},{q, sigma}}, {p1,p2}}; checkFormula[ex]
]

(*	roughly 17 seconds on a Core i7	*)
Print @ AbsoluteTiming[
Print ["C^{mu,nu,rho,sigma,delta}:"];
ex = {{{q, mu},{q, nu},{q, rho},{q, sigma},{q, delta}}, {p1,p2}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["D^{mu}:"];
ex = {{{q, mu}}, {p1,p2,p3}}; checkFormula[ex]
]

Print @ AbsoluteTiming[
Print ["D^{mu,nu}:"];
ex = {{{q, mu},{q, nu}}, {p1,p2,p3}}; checkFormula[ex]
]

(*	roughly 11 seconds on a Core i7	*)
Print @ AbsoluteTiming[
Print ["D^{mu,nu,rho}:"];
ex = {{{q, mu},{q, nu},{q, rho}}, {p1,p2,p3}}; checkFormula[ex]
]

(*	8-9 minutes on a Core i7	*)
Print @ AbsoluteTiming[
Print ["D^{mu,nu,rho,sigma}:"];
ex = {{{q, mu},{q, nu},{q, rho},{q, sigma}}, {p1,p2,p3}}; checkFormula[ex]
]
