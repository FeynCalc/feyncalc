(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCToPakForm														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  	Obtains a canonical (Pak) representation of the given
				FeynCalc integral 											*)

(* ------------------------------------------------------------------------ *)

FCToPakForm::usage =
"FCToPakForm[int, {p1, p2, ...}] determines a canonical UF-based representation \
for the scalar multi-loop integral int using the algorithm of Alexey Pak \
(arXiv:1111.0868).

The current implementation is based on the FindEquivalents function from \
FIRE 6 (arXiv:1901.07808)";

FCToPakForm::failmsg =
"Error! FCToPakForm has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCToPakForm`Private`"]

fctpfVerbose::usage = "";

Options[FCToPakForm] = {
	CharacteristicPolynomial	-> Function[{U,F}, U+F],
	Check						-> True,
	Collecting					-> True,
	FCE							-> False,
	FCI							-> False,
	FCVerbose 					-> False,
	Factoring					-> Factor,
	FinalSubstitutions			-> {},
	Function					-> Function[{U, F, pows, head, int}, {int, head[ExpandAll[U], ExpandAll[F], Transpose[pows]]}],
	Head						-> FCGV["PakFormHead"],
	Indexed						-> True,
	Names						-> FCGV["x"]
};


FCToPakForm[expr_List, lmoms_List, opts:OptionsPattern[]] :=
	FCToPakForm[#, lmoms, opts]&/@expr;

FCToPakForm[expr_/;Head[expr]=!=List, lmoms_List, OptionsPattern[]] :=
	Block[{	uPoly, fPoly, pows, mat, Q, J, tensorPart,
			tensorRank, rulePowers, pPoly, pVars, sigma,
			pVarsRepRule, powsReordered, powerMark, res, time,
			optFactoring},

		If[	OptionValue[FCVerbose] === False,
			fctpfVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fctpfVerbose = OptionValue[FCVerbose]];
		];

		optFactoring = OptionValue[Factoring];

		FCPrint[1, "FCToPakForm: Entering.", FCDoControl -> fctpfVerbose];
		FCPrint[3, "FCToPakForm: Entering with: ", expr, FCDoControl -> fctpfVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCToPakForm: Calling FCFeynmanPrepare.", FCDoControl -> fctpfVerbose];

		{uPoly, fPoly, pows, mat, Q, J, tensorPart, tensorRank} =
			FCFeynmanPrepare[expr, lmoms, FCI -> OptionValue[FCI], FinalSubstitutions -> OptionValue[FinalSubstitutions],
				Names -> OptionValue[Names], Indexed -> OptionValue[Indexed], Check->OptionValue[Check],
				Collecting -> OptionValue[Collecting]];
		FCPrint[1, "FCToPakForm: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctpfVerbose];

		If[optFactoring=!=False,
			time=AbsoluteTime[];
			FCPrint[1, "FCToPakForm: Factoring U and F polynomials.", FCDoControl -> fctpfVerbose];
			uPoly = optFactoring[uPoly];
			fPoly = optFactoring[fPoly];
			FCPrint[1, "FCToPakForm: Factoring done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctpfVerbose];

		];

		If[	tensorRank=!=0,
			Message[FCToPakForm::failmsg,"Tensor integrals are not supported"];
			Abort[]
		];

		rulePowers = Map[Rule[#[[1]], powerMark[#[[3]]] #[[1]]] &, pows];

		pPoly = OptionValue[CharacteristicPolynomial][uPoly,fPoly] /. rulePowers;
		pVars = First[Transpose[pows]];

		time=AbsoluteTime[];
		FCPrint[1, "FCToPakForm: Calling FCPakOrder.", FCDoControl -> fctpfVerbose];

		sigma = FCPakOrder[pPoly, pVars] // First;
		If[ !MatchQ[sigma,{__Integer}],
			Message[FCToPakForm::failmsg,"Failed to determine a unique ordering for this polynomial"];
			Abort[]
		];
		FCPrint[1, "FCToPakForm: FCPakOrder done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctpfVerbose];

		pVarsRepRule = Thread[Rule[pVars, Permute[pVars, Cycles[{sigma}]]]];
		FCPrint[2, "FCToPakForm: Reordering rule: ", pVarsRepRule, FCDoControl -> fctpfVerbose];
		powsReordered = Extract[pows, List /@ sigma];
		uPoly = uPoly /. pVarsRepRule;
		fPoly = fPoly /. pVarsRepRule;

		FCPrint[3, "FCToPakForm: Reordered propagators: ", powsReordered, FCDoControl -> fctpfVerbose];
		FCPrint[3, "FCToPakForm: Reordered U polynomial: ", uPoly, FCDoControl -> fctpfVerbose];
		FCPrint[3, "FCToPakForm: Reordered F polynomial: ", fPoly, FCDoControl -> fctpfVerbose];

		res = OptionValue[Function][uPoly, fPoly, powsReordered, OptionValue[Head], expr];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCToPakForm: Leaving.", FCDoControl -> fctpfVerbose];
		FCPrint[3, "FCToPakForm: Leaving with: ", res, FCDoControl -> fctpfVerbose];

		res
	];

powerMark[1]=1;

FCPrint[1,"FCToPakForm.m loaded."];
End[]
