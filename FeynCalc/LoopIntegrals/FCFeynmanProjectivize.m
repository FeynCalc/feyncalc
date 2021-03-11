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
"FCFeynmanProjectivize[int] checks if the given Feynman integral is \
projective. If this is not the case, the integral will be projectivized.";

FCFeynmanProjectivize::failmsg =
"Error! FCFeynmanProjectivize has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCFeynmanProjectivize`Private`"]

fcfprVerbose::usage="";


Options[FCFeynmanProjectivize] = {
	Assumptions	-> {},
	DivideBy	-> Automatic,
	FCVerbose 	-> False
};



FCFeynmanProjectivize[ex_, var_, OptionsPattern[]] :=
	Block[{xvars, ru, la, cru, check, res, fac, optDivideBy, optAssumptions},

		If [OptionValue[FCVerbose]===False,
			fcfprVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcfprVerbose=OptionValue[FCVerbose]
			];
		];

		optDivideBy = OptionValue[DivideBy];
		optAssumptions = OptionValue[Assumptions];

		Which[
			Head[var]===Symbol,
				xvars = Cases2[ex, var],
			Head[var]===List,
				xvars = var,
			_,
				Message[FCFeynmanProjectivize::failmsg, "Unknowns format of the second argument"];
				Abort[]
		];


		If[optDivideBy === Automatic,
			fac = 1/(Total[xvars]),
			fac = 1/optDivideBy
		];

		ru = Thread[Rule[xvars, fac xvars]];
		cru = Thread[Rule[xvars, la xvars]];

		check = FullSimplify[la^Length[xvars] (ex /. cru),	Assumptions -> Join[optAssumptions, {la > 0}]];

		If[	FreeQ[check, la],

			FCPrint[0,"FCFeynmanProjectivize: The integral is already projective, no further transformations are required.", FCDoControl->fcfprVerbose];
			Return[ex],

			FCPrint[0,"FCFeynmanProjectivize: The integral is not projective, trying to projectivize.", FCDoControl->fcfprVerbose];
		];


		res = (ex /. ru) (Total[xvars])^(-Length[xvars]);
		check = FullSimplify[la^Length[xvars] (res /. cru), Assumptions -> Join[optAssumptions, {la > 0}]];
		If[	FreeQ[check, la],

			FCPrint[0,"FCFeynmanProjectivize: Projective transformation successful, the integral is now projective.", FCDoControl->fcfprVerbose],

			Message[FCFeynmanProjectivize::failmsg, "Failed to projectivize the integral."];
			Abort[]
		];

		res
]


FCPrint[1,"FCFeynmanProjectivize.m loaded."];
End[]
