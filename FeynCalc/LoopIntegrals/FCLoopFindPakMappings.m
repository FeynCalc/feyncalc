(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindPakMappings											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Obtains a canonical (Pak) representation of the given
				FeynCalc integral 											*)

(* ------------------------------------------------------------------------ *)

FCLoopFindPakMappings::usage =
"FCLoopFindPakMappings[{int1, int2, ...}, {p1, p2, ...}] finds mappings between scalar
multiloop-integrals int1, int2, ... that depend on the loop momenta p1, p2, ...
using the algorithm of Alexey Pak (arXiv:1111.0868).

The current implementation is based on the FindEquivalents function from \
FIRE 6 (arXiv:1901.07808)";

FCLoopFindPakMappings::failmsg =
"Error! FCLoopFindPakMappings has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFindPakMappings`Private`"]

fcfpmVerbose::usage = "";

Options[FCLoopFindPakMappings] = {
	CharacteristicPolynomial	-> Function[{U,F}, U+F],
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	FinalSubstitutions			-> {},
	Function					-> Function[{U, F, charPoly, pows, head, int, sigma}, {int, head[ExpandAll[U], ExpandAll[F]]}]
};

FCLoopFindPakMappings[expr_, lmoms_List, OptionsPattern[]] :=
	Block[{	pakFormInts, res, time, x, pakHead, powerMark},

		If[	OptionValue[FCVerbose] === False,
			fcfpmVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fcfpmVerbose = OptionValue[FCVerbose]];
		];

		FCPrint[1, "FCLoopFindPakMappings: Entering.", FCDoControl -> fcfpmVerbose];
		FCPrint[3, "FCLoopFindPakMappings: Entering with: ", expr, FCDoControl -> fcfpmVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindPakMappings: Calling FCToPakForm.", FCDoControl -> fcfpmVerbose];
		pakFormInts = FCLoopToPakForm[#, lmoms, FCI->OptionValue[FCI], FinalSubstitutions->OptionValue[FinalSubstitutions],
			Check->False, Collecting->False, Names->x, CharacteristicPolynomial->OptionValue[CharacteristicPolynomial],
			Function->OptionValue[Function], Head->pakHead, Power->powerMark] & /@ expr;
		FCPrint[1, "FCLoopFindPakMappings: FCToPakForm done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfpmVerbose];

		FCPrint[3, "FCLoopFindPakMappings: Output of FCToPakForm: ", pakFormInts, FCDoControl->fcfpmVerbose];

		res = Reap[Sow [Sequence @@ #] & /@ pakFormInts, _, ##2 &][[2]];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCLoopFindPakMappings: Leaving.", FCDoControl -> fcfpmVerbose];
		FCPrint[3, "FCLoopFindPakMappings: Leaving with: ", res, FCDoControl -> fcfpmVerbose];

		res
	];

FCPrint[1,"FCLoopFindPakMappings.m loaded."];
End[]
