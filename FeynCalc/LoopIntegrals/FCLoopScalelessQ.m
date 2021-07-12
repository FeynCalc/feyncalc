(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopScalelessQ													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Detects scaleless loop integrals 							*)

(* ------------------------------------------------------------------------ *)

FCLoopScalelessQ::usage =
"FCLoopScalelessQ[int, {p1, p2, ...}] checks whether the loop integral int \
depending on the loop momenta p1, p2, ... is scaleless. Only integrals that \
admit a Feynman parametrization with proper U and F polynomials are supported. \

Cf. arXiv:1011.4863 and the PhD thesis of Jens Hoff \
(Hoff:2015kub, 10.5445/IR/1000047447) for the description of the underlying \
algorithm.";

FCLoopScalelessQ::failmsg =
"Error! FCLoopScalelessQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopScalelessQ`Private`"]

fclsVerbose::usage = "";

Options[FCLoopScalelessQ] = {
	Collecting			-> True,
	FCI					-> False,
	FCVerbose 			-> False,
	Factoring 			-> {Factor2, 5000},
	FinalSubstitutions	-> {},
	TimeConstrained 	-> 3
};

FCLoopScalelessQ[expr_, lmoms_List, OptionsPattern[]] :=
	Block[{	uPoly, fPoly, pows, mat, Q, J, tensorPart,
			tensorRank, res, time, x},

		If[	OptionValue[FCVerbose] === False,
			fclsVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fclsVerbose = OptionValue[FCVerbose]];
		];

		FCPrint[1, "FCLoopScalelessQ: Entering.", FCDoControl -> fclsVerbose];
		FCPrint[3, "FCLoopScalelessQ: Entering with: ", expr, FCDoControl -> fclsVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopScalelessQ: Calling FCFeynmanPrepare.", FCDoControl -> fclsVerbose];

		{uPoly, fPoly, pows, mat, Q, J, tensorPart, tensorRank} =
			FCFeynmanPrepare[expr, lmoms, FCI -> OptionValue[FCI], Names -> x, Check->False,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained],
				Factoring -> OptionValue[Factoring], FinalSubstitutions-> OptionValue[FinalSubstitutions]];
		FCPrint[1, "FCLoopScalelessQ: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclsVerbose];

		FCPrint[3, "FCLoopScalelessQ: U: ", uPoly, FCDoControl -> fclsVerbose];
		FCPrint[3, "FCLoopScalelessQ: F: ", fPoly, FCDoControl -> fclsVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopScalelessQ: Calling FCPakScalelessQ.", FCDoControl -> fclsVerbose];

		res = FCLoopPakScalelessQ[uPoly*fPoly,x];
		FCPrint[1, "FCLoopScalelessQ: FCPakScalelessQ done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclsVerbose];

		FCPrint[3, "FCLoopScalelessQ: Leaving.", FCDoControl -> fclsVerbose];
		FCPrint[3, "FCLoopScalelessQ: Leaving with: ", res, FCDoControl -> fclsVerbose];

		res
	];

FCPrint[1,"FCLoopScalelessQ.m loaded."];
End[]
