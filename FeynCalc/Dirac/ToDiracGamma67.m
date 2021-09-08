(* ::Package:: *)


(* :Title: ToDiracGamma67														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Introduces chirality projectors								*)

(* ------------------------------------------------------------------------ *)

ToDiracGamma67::usage =
"ToDiracGamma67[exp] substitutes $\\frac{1}{2} \\left(1 + \\gamma^5\\right)$ and
$\\frac{1}{2}\\left(1-\\gamma^5\\right)$ by the chirality projectors $\\gamma^6$
and $\\gamma^7$.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToDiracGamma67`Private`"]

Options[ToDiracGamma67] = {
	All 		-> False,
	DotSimplify -> False,
	FCE 		-> False,
	FCI 		-> False
};


ToDiracGamma67[a_ == b_, opts:OptionsPattern[]] :=
	ToDiracGamma67[a,opts] == ToDiracGamma67[b,opts];

ToDiracGamma67[expr_List, opts:OptionsPattern[]]:=
	ToDiracGamma67[#, opts]&/@expr;

ToDiracGamma67[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
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
			ex = DotSimplify[ex, FCI->True]
		];

		If[ OptionValue[FCE],
			ex = FCE[ex]
		];

		ex

]

FCPrint[1,"ToDiracGamma67.m loaded"];
End[]
