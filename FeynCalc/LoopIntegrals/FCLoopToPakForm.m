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
"FCLoopToPakForm[int, {p1, p2, ...}] determines a canonical $UF$-based
representation for the scalar multi-loop integral int that depend on the loop
momenta p1, p2, ... using the algorithm of Alexey Pak
[arXiv:1111.0868](https://arxiv.org/abs/1111.0868).

The current implementation is based on the FindEquivalents function from FIRE
6 [arXiv:1901.07808](https://arxiv.org/abs/1901.07808). FCLoopToPakForm is a
backend function used in FCLoopPakScalelessQ, FCLoopFindIntegralMappings,
FCLoopFindTopologyMappings etc.

It is also possible to invoke the function as FCLoopToPakForm[GLI[...],
FCTopology[...]] or FCLoopToPakForm[FCTopology[...]]. Notice that in this case
the value of the option FinalSubstitutions is ignored, as replacement rules
will be extracted directly from the definition of the topology.";

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
	Indexed						-> True,
	Names						-> FCGV["x"],
	Power						-> FCGV["PowerMark"]
};


FCLoopToPakForm[expr: {__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopToPakForm[expr, {FCGV["dummy"]}, opts];

FCLoopToPakForm[expr_FCTopology, opts:OptionsPattern[]] :=
	FCLoopToPakForm[expr, {FCGV["dummy"]}, opts];

FCLoopToPakForm[expr_, lmomsRaw_/; !OptionQ[lmomsRaw], OptionsPattern[]] :=
	Block[{	lmoms, res, time, optFinalSubstitutions, ex, tmp,
			optFactoring,optPowerMark, optCharacteristicPolynomial,
			optFCLoopPakOrder, notList=False},

		If[	OptionValue[FCVerbose] === False,
			fctpfVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fctpfVerbose = OptionValue[FCVerbose]];
		];

		optFactoring 				= OptionValue[Factoring];
		optPowerMark 				= OptionValue[Power];
		optCharacteristicPolynomial = OptionValue[CharacteristicPolynomial];
		optFinalSubstitutions 		= OptionValue[FinalSubstitutions];
		optFCLoopPakOrder 			= OptionValue[FCLoopPakOrder];

		FCPrint[1, "FCLoopToPakForm: Entering.", FCDoControl -> fctpfVerbose];
		FCPrint[3, "FCLoopToPakForm: Entering with: ", expr, FCDoControl -> fctpfVerbose];
		FCPrint[3, "FCLoopToPakForm: and: ", lmomsRaw, FCDoControl -> fctpfVerbose];


		If[	OptionValue[FCI],
			{ex, lmoms} = {expr,lmomsRaw},
			{ex, lmoms, optFinalSubstitutions} = FCI[{expr, lmomsRaw, optFinalSubstitutions}]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopToPakForm: Calling FCFeynmanPrepare.", FCDoControl -> fctpfVerbose];

		(*{uPoly, fPoly, pows, mat, Q, J, tensorPart, tensorRank} =*)
		If[	lmoms==={FCGV["dummy"]},
			lmoms=Sequence[]
		];

		Which[
			(*Single integral *)
			MatchQ[ex,_. _FeynAmpDenominator] || MatchQ[ex, (_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) | _FCTopology],
				notList = True;
				tmp =	FCFeynmanPrepare[ex, lmoms, FCI -> True, FinalSubstitutions -> optFinalSubstitutions,
				Names -> OptionValue[Names], Indexed -> OptionValue[Indexed], Check->OptionValue[Check],
				Collecting -> OptionValue[Collecting]];
				tmp = {tmp};
				ex = {ex},
			(*List of integrals *)
			MatchQ[ex, {(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) ..} | {__FCTopology}],
				tmp =	FCFeynmanPrepare[ex, lmoms, FCI -> True, FinalSubstitutions -> optFinalSubstitutions,
				Names -> OptionValue[Names], Indexed -> OptionValue[Indexed], Check->OptionValue[Check],
				Collecting -> OptionValue[Collecting]],
			(*List of integrals *)
			MatchQ[ex, {_. _FeynAmpDenominator ..}],
				tmp =	FCFeynmanPrepare[#, lmoms, FCI -> True, FinalSubstitutions -> optFinalSubstitutions,
				Names -> OptionValue[Names], Indexed -> OptionValue[Indexed], Check->OptionValue[Check],
				Collecting -> OptionValue[Collecting]]&/@ex,
			True,
				Message[FCLoopToPakForm::failmsg,"Failed to recognize the form of the input expression."];
				Abort[]
		];

		FCPrint[1, "FCLoopToPakForm: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctpfVerbose];

		FCPrint[3, "FCLoopToPakForm: Output of FCFeynmanPrepare: ", tmp, FCDoControl->fctpfVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopToPakForm: Calling pakProcess.", FCDoControl -> fctpfVerbose];

		tmp = pakProcess[#,{optFactoring,optPowerMark,optCharacteristicPolynomial, optFCLoopPakOrder}]&/@tmp;

		FCPrint[1, "FCLoopToPakForm: pakProcess done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctpfVerbose];

		If[	!FreeQ[tmp,pakProcess],
			Message[FCLoopToPakForm::failmsg,"Failed to process the output of FCFeynmanPrepare."];
			Abort[]
		];

		(* Function[{U, F, charPoly, pows, head, int, sigma}, {int, head[ExpandAll[charPoly], Transpose[pows]]}]*)
		res = MapThread[OptionValue[Function][Sequence@@(#1[[1;;4]]), OptionValue[Head], #2, #1[[5]]]&,{tmp,ex}];

		If[	notList,
			res = First[res]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCLoopToPakForm: Leaving.", FCDoControl -> fctpfVerbose];
		FCPrint[3, "FCLoopToPakForm: Leaving with: ", res, FCDoControl -> fctpfVerbose];

		res
	];

pakProcess[{uPolyRaw_, fPolyRaw_, powsRaw_List, matRaw_List, QRaw_List, JRaw_, tensorPartRaw_, tensorRankRaw_},
	{optFactoring_, optPowerMark_, optCharacteristicPolynomial_, optFCLoopPakOrder_}]:=
		Block[	{time, uPoly, fPoly, pows, mat, Q, J, tensorPart,
			tensorRank, rulePowers, pVarsRepRule, pPoly, pVars,
			sigma, powsReordered},

			{uPoly, fPoly, pows, mat, Q, J, tensorPart, tensorRank} =
				{uPolyRaw, fPolyRaw, powsRaw, matRaw, QRaw, JRaw, tensorPartRaw, tensorRankRaw};
			If[optFactoring=!=False,
				time=AbsoluteTime[];
				FCPrint[2, "FCLoopToPakForm: pakProcess: Factoring U and F polynomials.", FCDoControl -> fctpfVerbose];
				uPoly = optFactoring[uPoly];
				fPoly = optFactoring[fPoly];
				FCPrint[2, "FCLoopToPakForm: pakProcess: Factoring done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctpfVerbose];

			];

			If[	tensorRank=!=0,
				Message[FCLoopToPakForm::failmsg,"Tensor integrals are not supported"];
				Abort[]
			];

			rulePowers = Map[Rule[#[[1]], optPowerMark[#[[3]]] #[[1]]] &, pows] /. optPowerMark[1]->1;

			{uPoly, fPoly} = {uPoly, fPoly} /. rulePowers;

			pPoly = optCharacteristicPolynomial[uPoly,fPoly];

			FCPrint[2, "FCLoopToPakForm: pakProcess: pPoly: ", pPoly, FCDoControl -> fctpfVerbose];

			If[	optFCLoopPakOrder && (pPoly=!=0),
				pVars = First[Transpose[pows]];

				time=AbsoluteTime[];
				FCPrint[2, "FCLoopToPakForm: pakProcess: Calling FCPakOrder.", FCDoControl -> fctpfVerbose];

				sigma = FCLoopPakOrder[pPoly, pVars] // First;
				FCPrint[2, "FCLoopToPakForm: pakProcess: FCPakOrder done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctpfVerbose];

				If[ !MatchQ[sigma,{__Integer}],
					Message[FCLoopToPakForm::failmsg,"Failed to determine a unique ordering for this polynomial"];
					Abort[]
				];
				FCPrint[3, "FCLoopToPakForm: sigma: ", sigma, FCDoControl->fctpfVerbose];


				pVarsRepRule =  Thread[Rule[Extract[pVars, List /@ sigma], pVars]];

				FCPrint[3, "FCLoopToPakForm: Reordering rule: ", pVarsRepRule, FCDoControl -> fctpfVerbose];
				powsReordered = Extract[pows, List /@ sigma];
				uPoly = uPoly /. pVarsRepRule;
				fPoly = fPoly /. pVarsRepRule;
				pPoly = pPoly /. pVarsRepRule;

				FCPrint[3, "FCLoopToPakForm: Reordered propagators: ", powsReordered, FCDoControl -> fctpfVerbose];
				FCPrint[3, "FCLoopToPakForm: Reordered U polynomial: ", uPoly, FCDoControl -> fctpfVerbose];
				FCPrint[3, "FCLoopToPakForm: Reordered F polynomial: ", fPoly, FCDoControl -> fctpfVerbose],

				powsReordered = pows
			];
			{uPoly, fPoly, pPoly, powsReordered, sigma}
		];

FCPrint[1,"FCLoopToPakForm.m loaded."];
End[]
