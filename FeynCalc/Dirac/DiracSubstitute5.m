(* ::Package:: *)


(* :Title: DiracSubstitute5												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Substitutes DiracGamma[6] and DiracGamma[7] in terms of
				DiracGamma[5]												*)

(* ------------------------------------------------------------------------ *)

DiracSubstitute5::usage =
"DiracSubstitute5[exp] rewrites $\\gamma^5$ in terms of the chirality projectors
$\\gamma^6$ and $\\gamma^7$. DiracSubstitute5 is also an option of various
FeynCalc functions that handle Dirac algebra.";

DiracSubstitute5::failmsg =
"Error! DiracSubstitute5 has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracSubstitute5`Private`"];

Options[DiracSubstitute5] = {
	FCE -> False,
	FCI -> False
};

DiracSubstitute5[a_ == b_, opts:OptionsPattern[]] :=
	DiracSubstitute5[a,opts] == DiracSubstitute5[b,opts];

DiracSubstitute5[expr_List, opts:OptionsPattern[]]:=
	DiracSubstitute5[#, opts]&/@expr;

DiracSubstitute5[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, res, holdDOT},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex, {DiracGamma[5]}],
			Return[ex]
		];

		res = ex /. DiracGamma[5] :> DiracGamma[6]-DiracGamma[7];


		If[	!FreeQ2[res,{DiracGamma[5]}],
			Message[DiracSubstitute5::failmsg,"Failed to eliminate all occurences of DiracGamma[5]."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"DiracSubstitute5.m loaded"];
End[]
