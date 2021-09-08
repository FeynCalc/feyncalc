(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFeynmanProjectivize											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Projectivizes a Feynman integral							*)

(* ------------------------------------------------------------------------ *)

FCFeynmanProjectivize::usage =
"FCFeynmanProjectivize[int] checks if the given Feynman integral is a
projective form. If this is not the case, the integral will be projectivized.

Projectivity is a necessary condition for computing the integral with the aid
of the Cheng-Wu theorem";

FCFeynmanProjectivize::failmsg =
"Error! FCFeynmanProjectivize has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCFeynmanProjectivize`Private`"]

fcfprVerbose::usage="";


Options[FCFeynmanProjectivize] = {
	Assumptions	-> {},
	RandomPrime	-> 10^8,
	FCVerbose 	-> False
};



FCFeynmanProjectivize[ex_, var_, OptionsPattern[]] :=
	Block[{	xVars, ru, la, cru, check, res,
			optAssumptions, optRandomPrime, xVarsNum, kinVarsNum,
			aux, allVars, pow, kinVars, expVars},

		If [OptionValue[FCVerbose]===False,
			fcfprVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcfprVerbose=OptionValue[FCVerbose]
			];
		];

		optAssumptions	= OptionValue[Assumptions];
		optRandomPrime	= OptionValue[RandomPrime];

		Which[
			Head[var]===Symbol,
				xVars = Cases2[ex, var],
			Head[var]===List,
				xVars = var,
			_,
				Message[FCFeynmanProjectivize::failmsg, "Unknowns format of the second argument"];
				Abort[]
		];


		allVars = Variables2[Cases[ex /. Power -> pow, pow[x_, y_] :> {x, y}, Infinity] /. pow -> Power];
		expVars = Variables2[Cases[ex /. Power -> pow, pow[_, x_] :> x, Infinity]];
		kinVars = Complement[allVars,xVars,expVars];

		FCPrint[2, "FCFeynmanProjectivize: All variables present in the expression: " , allVars, FCDoControl->fcfprVerbose];

		FCPrint[1, "FCFeynmanProjectivize: Feynman parameter variables: ", xVars, FCDoControl->fcfprVerbose];
		FCPrint[1, "FCFeynmanProjectivize: Variables appearing in the exponents: ", expVars, FCDoControl->fcfprVerbose];
		FCPrint[1, "FCFeynmanProjectivize: Kinematic variables: ", kinVars, FCDoControl->fcfprVerbose];

		If[	Length[xVars]<2,
			Message[FCFeynmanProjectivize::failmsg, "Integrals with less than two integration variables are not supported."];
			Abort[]
		];

		If[	Sort[Join[xVars,expVars,kinVars]]=!=Sort[allVars],
			Message[FCFeynmanProjectivize::failmsg, "Something went wrong identifying different variable types."];
			Abort[]
		];

		xVarsNum 	= Table[RandomPrime[optRandomPrime],{i,1,Length[xVars]}];
		kinVarsNum	= Table[RandomPrime[optRandomPrime],{i,1,Length[kinVars]}];

		ru = Thread[Rule[xVars, xVars/(Total[xVars])]];
		cru = Join[Thread[Rule[xVars, la xVarsNum]],Thread[Rule[kinVars, kinVarsNum]]];

		FCPrint[2,"FCFeynmanProjectivize: Replacement rule for the projective transformation (if needed): ", ru, FCDoControl->fcfprVerbose];
		FCPrint[2,"FCFeynmanProjectivize: Replacement rule for numerical checks: ", cru, FCDoControl->fcfprVerbose];

		aux = Factor[la^Length[xVars] (ex /. cru)];

		FCPrint[3,"FCFeynmanProjectivize: Resulting numerical expression: " , aux, FCDoControl->fcfprVerbose];

		check = Simplify[la^Length[xVars] (ex /. cru),	Assumptions -> Join[optAssumptions, {la > 0}]];

		If[	FreeQ[check, la],

			FCPrint[0,"FCFeynmanProjectivize: The integral is already projective, no further transformations are required.", FCDoControl->fcfprVerbose];
			Return[ex],

			FCPrint[0,"FCFeynmanProjectivize: The integral is not projective, trying to projectivize.", FCDoControl->fcfprVerbose];
		];


		res = (ex /. ru) (Total[xVars])^(-Length[xVars]);
		FCPrint[1,"FCFeynmanProjectivize: Resulting integral: " , res, FCDoControl->fcfprVerbose];


		check = Simplify[la^Length[xVars] (res /. cru),	Assumptions -> Join[optAssumptions, {la > 0}]];
		If[	FreeQ[check, la],

			FCPrint[0,"FCFeynmanProjectivize: Projective transformation successful, the integral is now projective.", FCDoControl->fcfprVerbose],

			Message[FCFeynmanProjectivize::failmsg, "Failed to projectivize the integral."];
			Abort[]
		];

		res
]


FCPrint[1,"FCFeynmanProjectivize.m loaded."];
End[]
