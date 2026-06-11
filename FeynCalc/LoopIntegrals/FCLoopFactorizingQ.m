(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFactorizingQ												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  	Detects factorizing loop integrals

				Supports parallel evaluation [X]

*)

(* ------------------------------------------------------------------------ *)

FCLoopFactorizingQ::usage =
"FCLoopFactorizinQI[int, topo] checks whether the given loop integral
factorizes or not. The input can be made integrals in the GLI or FAD notation.";

FCLoopFactorizingQ::failmsg =
"Error! FCLoopFactorizingQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFactorizingQ`Private`"]

Options[FCLoopFactorizingQ] = {
	Collecting				-> True,
	FCI						-> False,
	FCParallelize			-> False,
	FCVerbose 				-> False,
	Factoring 				-> {Factor2, 5000},
	FCFeynmanParametrize	-> True,
	FinalSubstitutions		-> {},
	TimeConstrained 		-> 3
};


FCLoopFactorizingQ[expr: {__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopFactorizingQ[expr, {FCGV["dummy"]}, opts];

FCLoopFactorizingQ[expr_FCTopology, opts:OptionsPattern[]] :=
	FCLoopFactorizingQ[expr, {FCGV["dummy"]}, opts];

FCLoopFactorizingQ[expr_, lmomsRaw_/; !OptionQ[lmomsRaw], OptionsPattern[]] :=
	Block[{	uPoly, fPoly, res, time, x, lmoms, optFinalSubstitutions,
			ex, notList = False, tmp, posFactorizing, optVerbose, optFCParallelize},

		If[	OptionValue[FCVerbose] === False,
			optVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			optVerbose = OptionValue[FCVerbose]];
		];

		If[	Head[lmomsRaw]=!=List,
			Message[FCLoopFactorizingQ::failmsg,"The second argument must be a list of loop momenta"];
			Abort[]
		];

		optFinalSubstitutions = OptionValue[FinalSubstitutions];
		optFCParallelize = OptionValue[FCParallelize];

		FCPrint[1, "FCLoopFactorizingQ: Entering.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopFactorizingQ: Entering with: ", expr, FCDoControl -> optVerbose];

		If[	OptionValue[FCI],
			{ex, lmoms} = {expr,lmomsRaw},
			{ex, lmoms, optFinalSubstitutions} = FCI[{expr, lmomsRaw, optFinalSubstitutions}]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFactorizingQ: Calling FCFeynmanPrepare.", FCDoControl -> optVerbose];

		If[	lmomsRaw==={FCGV["dummy"]},
			lmoms=Sequence[]
		];

		Which[
			(*Single integral *)
			MatchQ[ex,_. _FeynAmpDenominator] || MatchQ[ex, _GLI | _FCTopology],
				notList = True;
				tmp =	FCFeynmanPrepare[ex, lmoms, FCI -> True, Names -> x, Check->False, FCParallelize->optFCParallelize,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained],
				Factoring -> OptionValue[Factoring], FinalSubstitutions -> optFinalSubstitutions, FCLoopGetEtaSigns -> False];
				tmp = {tmp};
				ex = {ex},
			(*List of integrals *)
			MatchQ[ex, {__GLI} | {__FCTopology}],
				tmp = FCFeynmanPrepare[ex, lmoms, FCI -> True, Names -> x, Check->False, FCParallelize->optFCParallelize,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained],
				Factoring -> OptionValue[Factoring], FinalSubstitutions-> optFinalSubstitutions, FCLoopGetEtaSigns -> False],
			(*List of integrals *)
			MatchQ[ex, {_. _FeynAmpDenominator ..}],
				tmp =	FCFeynmanPrepare[#, lmoms, FCI -> True, Names -> x, Check->False, FCParallelize->optFCParallelize,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained],
				Factoring -> OptionValue[Factoring], FinalSubstitutions-> optFinalSubstitutions, FCLoopGetEtaSigns -> False]&/@ex,
			True,
				Message[FCLoopFactorizingQ::failmsg,"Failed to recognize the form of the input expression."];
				Abort[]
		];

		FCPrint[1, "FCLoopFactorizingQ: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		tmp = Transpose[tmp];
		{uPoly, fPoly} = tmp[[1;;2]];

		FCPrint[3, "FCLoopFactorizingQ: U: ", uPoly, FCDoControl -> optVerbose];

		time=AbsoluteTime[];

		If[	$ParallelizeFeynCalc && optFCParallelize,

			FCPrint[1,"FCLoopFactorizingQ: Applying Factor in parallel." , FCDoControl->optVerbose];
			uPoly = ParallelMap[Factor[#]&,uPoly, DistributedContexts -> None,
				Method->"ItemsPerEvaluation" -> Ceiling[N[Length[uPoly]/$KernelCount]/10]];
			FCPrint[1, "FCLoopFactorizingSplit: Factor done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
			,

			FCPrint[1,"FCLoopFactorizingQ: Applying Factor." , FCDoControl->optVerbose];
			uPoly = Factor/@uPoly;
			FCPrint[1, "FCLoopFactorizingQ: Factor done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		];

		posFactorizing = Position[uPoly, zz_ /; (Head[zz] === Times), 1];

		FCPrint[1,"FCLoopFactorizingQ: Positions of factorizing integrals:", posFactorizing  , FCDoControl->optVerbose];

		res = ConstantArray[False,Length[ex]];
		res = ReplacePart[res, posFactorizing -> True];

		If[	notList,
			res = First[res]
		];

		FCPrint[3, "FCLoopFactorizingQ: Leaving.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopFactorizingQ: Leaving with: ", res, FCDoControl -> optVerbose];

		res
	];

FCPrint[1,"FCLoopFactorizingQ.m loaded."];
End[]
