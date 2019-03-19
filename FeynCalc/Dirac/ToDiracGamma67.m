(* ::Package:: *)


(* :Title: ToDiracGamma67														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Introduces chirality projectors								*)

(* ------------------------------------------------------------------------ *)

ToDiracGamma67::usage =
"ToDiracGamma67[exp] substitutes 1/2(1 + GA[5]) and 1/2(1 - GA[5]) by \
DiracGamma[6] and DiracGamma[7].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToDiracGamma67`Private`"]

Options[ToDiracGamma67] = {
	All -> False,
	FCI -> False,
	FCE -> False,
	DotSimplify -> False
}

ToDiracGamma67[expr_, OptionsPattern[]] :=
	Block[{ex},

		If[ OptionValue[FCI],
				ex = expr,
				ex = FCI[expr]
		];


		ex = ex /.{
			1/2+DiracGamma[5]/2 -> DiracGamma[6],
			1/2-DiracGamma[5]/2 -> DiracGamma[7],
			1+DiracGamma[5] -> 2 DiracGamma[6],
			1-DiracGamma[5] -> 2 DiracGamma[7]
		};

		If[ OptionValue[All],
			ex = ex /. DiracGamma[5] -> DiracGamma[6] - DiracGamma[7]
		];

		If[ OptionValue[DotSimplify],
			ex = DotSimplify[ex, FCI->False]
		];

		If[ OptionValue[FCE],
			ex = FCE[ex]
		];


		ex

]

FCPrint[1,"ToDiracGamma67.m loaded"];
End[]
