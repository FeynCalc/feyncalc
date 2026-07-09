(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopToPakForm													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*	:Summary:  	Obtains a canonical (Pak) representation of the given
				FeynCalc integral

				Supports parallel evaluation [X]
*)

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

Options[FCLoopToPakForm] = {
	CharacteristicPolynomial	-> Function[{U,F}, U+F],
	Check						-> True,
	Collecting					-> True,
	FCE							-> False,
	FCI							-> False,
	FCParallelize				-> False,
	FCLoopPakOrder				-> True,
	FCVerbose 					-> False,
	Factoring					-> Factor,
	FinalSubstitutions			-> {},
	Function					-> Function[{U, F, charPoly, pows, head, int, sigma}, {int, head[ExpandAll[charPoly], Transpose[pows]]}],
	"FunctionForAllSigmas"		-> Function[{func,head,listOfLists, int},
									Block[{tmp,aux},
										tmp = Map[func[Sequence@@(#[[1;;4]]), head, int, #[[5]]]&,listOfLists];
										tmp=Transpose[tmp][[2]];
										{int,tmp}
										]],
	Head						-> FCGV["PakFormHead"],
	Indexed						-> True,
	LightPak					-> False,
	Names						-> FCGV["x"],
	Power						-> FCGV["PowerMark"],
	Select						-> First
};




buildFinalResult[func_,head_,{li__List},int_]:=
	Block[{tmp,aux},
		tmp = Map[func[Sequence@@(#[[1;;4]]), head, int, #[[5]]]&,{li}];
		tmp=Transpose[tmp][[2]];
		{int,tmp}
	];

FCLoopToPakForm[expr: {__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopToPakForm[expr, {FCGV["dummy"]}, opts];

FCLoopToPakForm[expr_FCTopology, opts:OptionsPattern[]] :=
	FCLoopToPakForm[expr, {FCGV["dummy"]}, opts];

FCLoopToPakForm[expr_, lmomsRaw_/; !OptionQ[lmomsRaw], OptionsPattern[]] :=
	Block[{	lmoms, res, time, optFinalSubstitutions, ex, tmp, optSelect, optVerbose,
			optFactoring,optPowerMark, optCharacteristicPolynomial, optLightPak,
			optFCLoopPakOrder, notList=False, optFCParallelize, optFunctionForAllSigmas},

		If[	OptionValue[FCVerbose] === False,
			optVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			optVerbose = OptionValue[FCVerbose]];
		];

		optFactoring 				= OptionValue[Factoring];
		optPowerMark 				= OptionValue[Power];
		optCharacteristicPolynomial = OptionValue[CharacteristicPolynomial];
		optFinalSubstitutions 		= OptionValue[FinalSubstitutions];
		optFCLoopPakOrder 			= OptionValue[FCLoopPakOrder];
		optLightPak 				= OptionValue[LightPak];
		optSelect 					= OptionValue[Select];
		optFCParallelize			= OptionValue[FCParallelize];
		optFunctionForAllSigmas		= OptionValue["FunctionForAllSigmas"];

		FCPrint[1, "FCLoopToPakForm: Entering.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopToPakForm: Entering with: ", expr, FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopToPakForm: and: ", lmomsRaw, FCDoControl -> optVerbose];


		If[	OptionValue[FCI],
			{ex, lmoms} = {expr,lmomsRaw},
			{ex, lmoms, optFinalSubstitutions} = FCI[{expr, lmomsRaw, optFinalSubstitutions}]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopToPakForm: Calling FCFeynmanPrepare.", FCDoControl -> optVerbose];

		(*{uPoly, fPoly, pows, mat, Q, J, tensorPart, tensorRank} =*)
		If[	lmoms==={FCGV["dummy"]},
			lmoms=Sequence[]
		];

		Which[
			(*List of integrals, the first condition avoids the "Recursion limit exceeded; positive match might be missed" error *)
			MatchQ[ex, {__GLI}] || MatchQ[ex, {(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) ..} | {__FCTopology}],
				FCPrint[1, "FCLoopToPakForm: We are dealing with a list of GLIs.", FCDoControl -> optVerbose];
				tmp =	FCFeynmanPrepare[ex, lmoms, FCI -> True, FinalSubstitutions -> optFinalSubstitutions,
				Names -> OptionValue[Names], Indexed -> OptionValue[Indexed], Check->OptionValue[Check],
				Collecting -> OptionValue[Collecting], FCLoopGetEtaSigns -> False, FCParallelize -> optFCParallelize],
			(*Single integral *)
			MatchQ[ex,_. _FeynAmpDenominator] || MatchQ[ex, (_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) | _FCTopology],
				notList = True;
				FCPrint[1, "FCLoopToPakForm: We are dealing with a single integral.", FCDoControl -> optVerbose];
				tmp =	FCFeynmanPrepare[ex, lmoms, FCI -> True, FinalSubstitutions -> optFinalSubstitutions,
				Names -> OptionValue[Names], Indexed -> OptionValue[Indexed], Check->OptionValue[Check],
				Collecting -> OptionValue[Collecting], FCLoopGetEtaSigns -> False, FCParallelize -> optFCParallelize];
				tmp = {tmp};
				ex = {ex},
			(*List of integrals *)
			MatchQ[ex, {_. _FeynAmpDenominator ..}],
				FCPrint[1, "FCLoopToPakForm: We are dealing with a list of integrals.", FCDoControl -> optVerbose];
				tmp =	FCFeynmanPrepare[#, lmoms, FCI -> True, FinalSubstitutions -> optFinalSubstitutions,
				Names -> OptionValue[Names], Indexed -> OptionValue[Indexed], Check->OptionValue[Check],
				Collecting -> OptionValue[Collecting], FCLoopGetEtaSigns -> False, FCParallelize -> optFCParallelize]&/@ex,
			True,
				Message[FCLoopToPakForm::failmsg,"Failed to recognize the form of the input expression."];
				Abort[]
		];

		FCPrint[1, "FCLoopToPakForm: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		FCPrint[3, "FCLoopToPakForm: Output of FCFeynmanPrepare: ", tmp, FCDoControl->optVerbose];


		time=AbsoluteTime[];


		If[	$ParallelizeFeynCalc && optFCParallelize,
			(*TODO: Don't use "CoarsestGrained" *)
			FCPrint[1, "FCLoopToPakForm: Calling pakProcess in parallel.", FCDoControl -> optVerbose];
			With[{xxx = {optFactoring,optPowerMark,optCharacteristicPolynomial, optFCLoopPakOrder,optSelect,optLightPak,optVerbose}},
				ParallelEvaluate[FCParallelContext`FCLoopToPakForm`pakProcessOptions = xxx;, DistributedContexts -> None]];
			tmp = ParallelMap[pakProcess[#,FCParallelContext`FCLoopToPakForm`pakProcessOptions]&,tmp, DistributedContexts -> None, Method -> "CoarsestGrained"],

			FCPrint[1, "FCLoopToPakForm: Calling pakProcess.", FCDoControl -> optVerbose];
			tmp = pakProcess[#,{optFactoring,optPowerMark,optCharacteristicPolynomial, optFCLoopPakOrder,optSelect,optLightPak,optVerbose}]&/@tmp
		];
		FCPrint[1, "FCLoopToPakForm: pakProcess done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		If[	!FreeQ[tmp,pakProcess],
			Message[FCLoopToPakForm::failmsg,"Failed to process the output of FCFeynmanPrepare."];
			Abort[]
		];


		time=AbsoluteTime[];

		(*We need to process the output differently depending on the value of the select option*)

		If[	$ParallelizeFeynCalc,

			FCPrint[1, "FCLoopToPakForm: Building up the final result in parallel.", FCDoControl -> optVerbose];
			With[{xxx = OptionValue[Function], yyy= OptionValue[Head], zzz = optFunctionForAllSigmas},
				ParallelEvaluate[(	FCParallelContext`FCLoopToPakForm`optValFunction = xxx;
									FCParallelContext`FCLoopToPakForm`optValHead = yyy;
									FCParallelContext`FCLoopToPakForm`optFunctionForAllSigmas = zzz;
									), DistributedContexts -> None]];

			res = ParallelMap[buildFinalResult[FCParallelContext`FCLoopToPakForm`optValFunction,
				FCParallelContext`FCLoopToPakForm`optValHead, #[[1]], #[[2]],
					FCParallelContext`FCLoopToPakForm`optFunctionForAllSigmas]&,Transpose[{tmp,ex}],
				DistributedContexts -> None, Method -> "CoarsestGrained"],

			FCPrint[1, "FCLoopToPakForm: Building up the final result.", FCDoControl -> optVerbose];

			res = MapThread[buildFinalResult[OptionValue[Function], OptionValue[Head], #1, #2, optFunctionForAllSigmas]&,{tmp,ex}];
		];

		FCPrint[1, "FCLoopToPakForm: Done building up the final result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		If[	notList,
			res = First[res]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCLoopToPakForm: Leaving.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopToPakForm: Leaving with: ", res, FCDoControl -> optVerbose];

		res
	];

buildFinalResult[func_,head_,{uPoly_/;Head[uPoly]=!=List, fPoly_, pPoly_, pows_, sigma_},int_, (*funcForAllSigmas*)_]:=
	func[uPoly, fPoly, pPoly, pows, head, int, sigma];

buildFinalResult[func_,head_,{li__List},int_, funcForAllSigmas_]:=
	funcForAllSigmas[func,head,{li}, int];

pakProcess[{uPolyRaw_, fPolyRaw_, powsRaw_List, matRaw_List, QRaw_List, JRaw_, tensorPartRaw_, tensorRankRaw_},
	{optFactoring_, optPowerMark_, optCharacteristicPolynomial_, optFCLoopPakOrder_, optSelect_, optLightPak_, optVerbose_}]:=
		Block[	{time, uPoly, fPoly, pows, mat, Q, J, tensorPart,
			tensorRank, rulePowers, pVarsRepRule, pPoly, pVars,
			sigma, powsReordered,res},

			{uPoly, fPoly, pows, mat, Q, J, tensorPart, tensorRank} =
				{uPolyRaw, fPolyRaw, powsRaw, matRaw, QRaw, JRaw, tensorPartRaw, tensorRankRaw};
			If[optFactoring=!=False,
				time=AbsoluteTime[];
				FCPrint[2, "FCLoopToPakForm: pakProcess: Factoring U and F polynomials.", FCDoControl -> optVerbose];
				uPoly = optFactoring[uPoly];
				fPoly = optFactoring[fPoly];
				FCPrint[2, "FCLoopToPakForm: pakProcess: Factoring done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

			];

			If[	tensorRank=!=0,
				Message[FCLoopToPakForm::failmsg,"Tensor integrals are not supported"];
				Abort[]
			];

			rulePowers = Map[Rule[#[[1]], optPowerMark[#[[3]]] #[[1]]] &, pows] /. optPowerMark[1]->1;

			FCPrint[2, "FCLoopToPakForm: pakProcess: rulePowers: ", rulePowers, FCDoControl -> optVerbose];

			{uPoly, fPoly} = {uPoly, fPoly} /. rulePowers;

			pPoly = optCharacteristicPolynomial[uPoly,fPoly];

			FCPrint[2, "FCLoopToPakForm: pakProcess: pPoly: ", pPoly, FCDoControl -> optVerbose];

			If[	optFCLoopPakOrder && (pPoly=!=0),

				pVars = First[Transpose[pows]];

				time=AbsoluteTime[];
				FCPrint[2, "FCLoopToPakForm: pakProcess: Calling FCPakOrder.", FCDoControl -> optVerbose];

				sigma = FCLoopPakOrder[pPoly, pVars, LightPak->optLightPak];

				FCPrint[2, "FCLoopToPakForm: pakProcess: FCPakOrder done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

				FCPrint[3, "FCLoopToPakForm: All sigmas: ", sigma, FCDoControl->optVerbose];

				If[	!TrueQ[optSelect===All],

					(*Only one sigma is selected*)
					sigma = optSelect[sigma];
					FCPrint[3, "FCLoopToPakForm: Selected sigma: ", sigma, FCDoControl->optVerbose];
					res = reorderVariables[uPoly,fPoly,pPoly,pows,pVars,sigma];

					FCPrint[3, "FCLoopToPakForm: Reordered propagators: ", powsReordered, FCDoControl -> optVerbose];
					FCPrint[3, "FCLoopToPakForm: Reordered U polynomial: ", uPoly, FCDoControl -> optVerbose];
					FCPrint[3, "FCLoopToPakForm: Reordered F polynomial: ", fPoly, FCDoControl -> optVerbose],

					(*All sigmas are taken*)
					res = Map[reorderVariables[uPoly,fPoly,pPoly,pows,pVars,#]&,sigma]
				],
				res = {uPoly, fPoly, pPoly, pows, sigma}
			];
			res
		];

reorderVariables[uRaw_,fRaw_,pRaw_,pows_,pVars_,sigma_]:=
	Block[{uPoly, fPoly, pPoly, pVarsRepRule, powsReordered},
		If[ !MatchQ[sigma,{__Integer}],
			Message[FCLoopToPakForm::failmsg,"Failed to determine a unique ordering for this polynomial"];
			Abort[]
		];
		pVarsRepRule =  Thread[Rule[Extract[pVars, List /@ sigma], pVars]];
		powsReordered = Extract[pows, List /@ sigma] /. Dispatch[pVarsRepRule];

		{uPoly,fPoly,pPoly} = {uRaw,fRaw,pRaw} /. Dispatch[pVarsRepRule];

		{uPoly, fPoly, pPoly, powsReordered, sigma}

	];

FCPrint[1,"FCLoopToPakForm.m loaded."];
End[]
