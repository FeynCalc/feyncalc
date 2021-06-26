(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFindPakMappings														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  	Obtains a canonical (Pak) representation of the given
				FeynCalc integral 											*)

(* ------------------------------------------------------------------------ *)

FCFindPakMappings::usage =
"FCFindPakMappings[{int1, int2, ...}, {p1, p2, ...}] finds mappings between scalar
multiloop-integrals int1, int2, ... that depend on the loop momenta p1, p2, ...
using using the algorithm of Alexey Pak (arXiv:1111.0868).

The current implementation is based on the FindEquivalents function from \
FIRE 6 (arXiv:1901.07808)";

FCFindPakMappings::failmsg =
"Error! FCFindPakMappings has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCFindPakMappings`Private`"]

fcfpmVerbose::usage = "";

Options[FCFindPakMappings] = {
	CharacteristicPolynomial	-> Function[{U,F}, U+F],
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	FinalSubstitutions			-> {},
	Function					-> Function[{U, F, pows, head, int}, {int, head[ExpandAll[U], ExpandAll[F], Transpose[pows]]}]
};

FCFindPakMappings[expr_, lmoms_List, OptionsPattern[]] :=
	Block[{	pakFormInts, res, time, x},

		If[	OptionValue[FCVerbose] === False,
			fcfpmVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fcfpmVerbose = OptionValue[FCVerbose]];
		];

		FCPrint[1, "FCFindPakMappings: Entering.", FCDoControl -> fcfpmVerbose];
		FCPrint[3, "FCFindPakMappings: Entering with: ", expr, FCDoControl -> fcfpmVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCFindPakMappings: Calling FCToPakForm.", FCDoControl -> fcfpmVerbose];
		pakFormInts = FCToPakForm[#, lmoms, FCI->OptionValue[FCI], FinalSubstitutions->OptionValue[FinalSubstitutions],
			Check->False, Collecting->False, Names->x, CharacteristicPolynomial->OptionValue[CharacteristicPolynomial],
			Function->OptionValue[Function]] & /@ expr;
		FCPrint[1, "FCFindPakMappings: FCToPakForm done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfpmVerbose];

		res = Reap[Sow [Sequence @@ #] & /@ pakFormInts, _, ##2 &][[2]];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCFindPakMappings: Leaving.", FCDoControl -> fcfpmVerbose];
		FCPrint[3, "FCFindPakMappings: Leaving with: ", res, FCDoControl -> fcfpmVerbose];

		res
	];

FCPrint[1,"FCFindPakMappings.m loaded."];
End[]
