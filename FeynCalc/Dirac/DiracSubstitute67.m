(* ::Package:: *)


(* :Title: DiracSubstitute67												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	Substitutes DiracGamma[6] and DiracGamma[7] in terms of
				DiracGamma[5]												*)

(* ------------------------------------------------------------------------ *)

DiracSubstitute67::usage =
"DiracSubstitute67[exp] inserts in exp the definitions of \
the chirality projectors DiracGamma[6] and DiracGamma[7]. \
DiracSubstitute67 is also an option of various FeynCalc functions \
that handle Dirac algebra.";

DiracSubstitute67::failmsg =
"Error! DiracSubstitute67 has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracSubstitute67`Private`"];

Options[DiracSubstitute67] = {
	FCI -> False,
	FCE -> False
};

DiracSubstitute67[expr_, OptionsPattern[]] :=
	Block[{ex, res, holdDOT},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex, {DiracGamma[6],DiracGamma[7]}],
			Return[ex]
		];

		res = ex /.  {
			DiracGamma[6] :> (1/2 + DiracGamma[5]/2),
			DiracGamma[7] :> (1/2 - DiracGamma[5]/2)
		};


		If[	!FreeQ2[res,{DiracGamma[6],DiracGamma[7]}],
			Message[DiracSubstitute67::failmsg,"Failed to eliminate all occurences of chirality projectors."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"DiracSubstitute67.m loaded"];
End[]
