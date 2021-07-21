(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopToPakForm													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Obtains a canonical (Pak) representation of the given
				FeynCalc integral 											*)

(* ------------------------------------------------------------------------ *)

FCLoopToPakForm::usage =
"FCLoopToPakForm[int, {p1, p2, ...}] determines a canonical UF-based representation \
for the scalar multi-loop integral int using the algorithm of Alexey Pak \
(arXiv:1111.0868).

The current implementation is based on the FindEquivalents function from \
FIRE 6 (arXiv:1901.07808)";

FCLoopToPakForm::failmsg =
"Error! FCLoopToPakForm has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopToPakForm`Private`"]

fctpfVerbose::usage = "";

Options[FCLoopToPakForm] = {
	CharacteristicPolynomial	-> Function[{U,F}, U+F],
	Check						-> True,
	Collecting					-> True,
	FCE							-> False,
	FCI							-> False,
	FCLoopPakOrder				-> True,
	FCVerbose 					-> False,
	Factoring					-> Factor,
	FinalSubstitutions			-> {},
	Function					-> Function[{U, F, charPoly, pows, head, int, sigma}, {int, head[ExpandAll[charPoly], Transpose[pows]]}],
	Head						-> FCGV["PakFormHead"],
	Power						-> FCGV["PowerMark"],
	Indexed						-> True,
	Names						-> FCGV["x"]
};

FCLoopToPakForm[expr_, lmoms_List, OptionsPattern[]] :=
	Block[{	uPoly, fPoly, pows, mat, Q, J, tensorPart,
			tensorRank, rulePowers, pPoly, pVars, sigma,
			pVarsRepRule, powsReordered, optPowerMark, res, time,
			optFactoring, optFinalSubstitutions, ex},

		If[	OptionValue[FCVerbose] === False,
			fctpfVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fctpfVerbose = OptionValue[FCVerbose]];
		];

		optFactoring = OptionValue[Factoring];
		optPowerMark = OptionValue[Power];
		optFinalSubstitutions = OptionValue[FinalSubstitutions];

		FCPrint[1, "FCLoopToPakForm: Entering.", FCDoControl -> fctpfVerbose];
		FCPrint[3, "FCLoopToPakForm: Entering with: ", expr, FCDoControl -> fctpfVerbose];


		If[	OptionValue[FCI],
			ex = expr,
			{ex,optFinalSubstitutions} = FCI[{expr,optFinalSubstitutions}]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopToPakForm: Calling FCFeynmanPrepare.", FCDoControl -> fctpfVerbose];

		{uPoly, fPoly, pows, mat, Q, J, tensorPart, tensorRank} =
			FCFeynmanPrepare[ex, lmoms, FCI -> True, FinalSubstitutions -> optFinalSubstitutions,
				Names -> OptionValue[Names], Indexed -> OptionValue[Indexed], Check->OptionValue[Check],
				Collecting -> OptionValue[Collecting]];
		FCPrint[1, "FCLoopToPakForm: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctpfVerbose];

		If[optFactoring=!=False,
			time=AbsoluteTime[];
			FCPrint[1, "FCLoopToPakForm: Factoring U and F polynomials.", FCDoControl -> fctpfVerbose];
			uPoly = optFactoring[uPoly];
			fPoly = optFactoring[fPoly];
			FCPrint[1, "FCLoopToPakForm: Factoring done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctpfVerbose];

		];

		If[	tensorRank=!=0,
			Message[FCLoopToPakForm::failmsg,"Tensor integrals are not supported"];
			Abort[]
		];

		rulePowers = Map[Rule[#[[1]], optPowerMark[#[[3]]] #[[1]]] &, pows] /. optPowerMark[1]->1;

		pPoly = OptionValue[CharacteristicPolynomial][uPoly,fPoly] /. rulePowers;

		If[	OptionValue[FCLoopPakOrder],
			pVars = First[Transpose[pows]];

			time=AbsoluteTime[];
			FCPrint[1, "FCLoopToPakForm: Calling FCPakOrder.", FCDoControl -> fctpfVerbose];

			sigma = FCLoopPakOrder[pPoly, pVars] // First;
			If[ !MatchQ[sigma,{__Integer}],
				Message[FCLoopToPakForm::failmsg,"Failed to determine a unique ordering for this polynomial"];
				Abort[]
			];
			FCPrint[1, "FCLoopToPakForm: sigma: ", sigma, FCDoControl->fctpfVerbose];

			FCPrint[1, "FCLoopToPakForm: Calling FCPakOrder.", FCDoControl -> fctpfVerbose];

			pVarsRepRule =  Thread[Rule[Extract[pVars, List /@ sigma], pVars]];

			FCPrint[2, "FCLoopToPakForm: Reordering rule: ", pVarsRepRule, FCDoControl -> fctpfVerbose];
			powsReordered = Extract[pows, List /@ sigma];
			uPoly = uPoly /. pVarsRepRule;
			fPoly = fPoly /. pVarsRepRule;
			pPoly = pPoly /. pVarsRepRule;

			FCPrint[3, "FCLoopToPakForm: Reordered propagators: ", powsReordered, FCDoControl -> fctpfVerbose];
			FCPrint[3, "FCLoopToPakForm: Reordered U polynomial: ", uPoly, FCDoControl -> fctpfVerbose];
			FCPrint[3, "FCLoopToPakForm: Reordered F polynomial: ", fPoly, FCDoControl -> fctpfVerbose],

			powsReordered = pows
		];
		(* Function[{U, F, charPoly, pows, head, int, sigma}, {int, head[ExpandAll[charPoly], Transpose[pows]]}]*)
		res = OptionValue[Function][uPoly, fPoly, pPoly, powsReordered, OptionValue[Head], ex, sigma];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCLoopToPakForm: Leaving.", FCDoControl -> fctpfVerbose];
		FCPrint[3, "FCLoopToPakForm: Leaving with: ", res, FCDoControl -> fctpfVerbose];

		res
	];

FCPrint[1,"FCLoopToPakForm.m loaded."];
End[]
