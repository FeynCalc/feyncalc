(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCTraceFactor														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:	Factors traces using linearity

				Supports parallel evaluation [X]
*)

(* ------------------------------------------------------------------------ *)

FCTraceFactor::usage =
"FCTraceFactor[expr] factors out all expressions inside a trace to which the
trace doesn't apply. For example, all objects that are not Dirac matrices can
be safely factored out from every Dirac trace.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCTraceFactor`Private`"]

Options[FCTraceFactor] = {
	FCI 			-> False,
	FCE 			-> False,
	FCParallelize	-> False,
	FCVerbose		-> False
};

FCTraceFactor[expr_, OptionsPattern[]] :=
	Block[ {ex, moms,res, diracTraces, pauliTraces, colorTraces,
			ruleDirac, rulePauli, ruleColor, diracTracesEval,
			pauliTracesEval, colorTracesEval, fctfVerbose, time,
			optFCParallelize},

		If [OptionValue[FCVerbose]===False,
			fctfVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fctfVerbose=OptionValue[FCVerbose]
			];
		];

		optFCParallelize = OptionValue[FCParallelize];

		FCPrint[1, "FCTraceFactor: Entering.", FCDoControl->fctfVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[ FreeQ2[ex,{DiracTrace,PauliTrace,SUNTrace}],
			Return[ex]
		];

		diracTraces = Cases2[ex, DiracTrace];
		pauliTraces = Cases2[ex, PauliTrace];
		colorTraces = Cases2[ex, SUNTrace];

		time = AbsoluteTime[];
		If[ diracTraces =!= {},
			If[	$ParallelizeFeynCalc && optFCParallelize,
				FCPrint[1, "FCTraceFactor: Applying diracTracefactor in parallel.", FCDoControl->fctfVerbose];
				diracTracesEval = ParallelMap[diracTracefactor[#]&,diracTraces,
					DistributedContexts -> None, Method->"ItemsPerEvaluation" -> Ceiling[N[Length[expr]/$KernelCount]/10]];
				FCPrint[1, "FCTraceFactor: Done applying diracTracefactor in parallel, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctfVerbose];
				ruleDirac = Thread[diracTraces -> diracTracesEval],

				FCPrint[1, "FCTraceFactor: Applying diracTracefactor.", FCDoControl->fctfVerbose];
				ruleDirac = Thread[diracTraces -> diracTracefactor[diracTraces]],
				FCPrint[1, "FCTraceFactor: Done applying diracTracefactor, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctfVerbose]
			]
		];

		time = AbsoluteTime[];
		If[ pauliTraces =!= {},
			If[	$ParallelizeFeynCalc && optFCParallelize,
				FCPrint[1, "FCTraceFactor: Applying pauliTracefactor in parallel.", FCDoControl->fctfVerbose];
				pauliTracesEval = ParallelMap[pauliTracefactor[#]&,pauliTraces,
					DistributedContexts -> None, Method->"ItemsPerEvaluation" -> Ceiling[N[Length[expr]/$KernelCount]/10]];
				FCPrint[1, "FCTraceFactor: Done applying pauliTracefactor in parallel, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctfVerbose];
				rulePauli = Thread[pauliTraces -> pauliTracesEval],

				FCPrint[1, "FCTraceFactor: Applying pauliTracefactor.", FCDoControl->fctfVerbose];
				rulePauli = Thread[pauliTraces -> pauliTracefactor[pauliTraces]];
				FCPrint[1, "FCTraceFactor: Done applying pauliTracefactor, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctfVerbose]
			]
		];

		time = AbsoluteTime[];
		If[ colorTraces =!= {},
			If[	$ParallelizeFeynCalc && optFCParallelize,
				FCPrint[1, "FCTraceFactor: Applying colorTracefactor.", FCDoControl->fctfVerbose];
				colorTracesEval = ParallelMap[colorTracefactor[#]&,colorTraces,
					DistributedContexts -> None, Method->"ItemsPerEvaluation" -> Ceiling[N[Length[expr]/$KernelCount]/10]];
				FCPrint[1, "FCTraceFactor: Done applying colorTracefactor in parallel, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctfVerbose];
				ruleColor = Thread[colorTraces -> colorTracesEval],

				FCPrint[1, "FCTraceFactor: Applying colorTracefactor in parallel.", FCDoControl->fctfVerbose];
				ruleColor = Thread[colorTraces -> colorTracefactor[colorTraces]];
				FCPrint[1, "FCTraceFactor: Done applying colorTracefactor, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctfVerbose]
			]
		];

		res = ex;

		If[ diracTraces =!= {},
			res = res /. Dispatch[ruleDirac]
		];

		If[ pauliTraces=!={},
			res = res /. Dispatch[rulePauli]
		];

		If[ colorTraces =!= {},
			res = res /. Dispatch[ruleColor]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCTraceFactor: Leaving.", FCDoControl->fctfVerbose];

		res
	];

diracTracefactor[x_] :=
	x /. DOT -> holdDOT /. DiracTrace->factorDirac /. factorDirac[] -> DiracTrace[1] /.
	holdDOT[] -> Sequence[] /.
	factorDirac -> DiracTrace /. holdDOT->DOT;

pauliTracefactor[x_] :=
	x /. DOT -> holdDOT /. PauliTrace->factorPauli /. factorPauli[] -> PauliTrace[1] /.
	holdDOT[] -> Sequence[] /.
	factorPauli -> PauliTrace /. holdDOT->DOT;

colorTracefactor[x_] :=
	x /. DOT -> holdDOT /. SUNTrace->factorColor /. factorColor[] -> SUNTrace[1] /.
	holdDOT[] -> Sequence[] /.
	factorColor -> SUNTrace /. holdDOT->DOT;

holdDOT[a___,b_,c___]:=
	b holdDOT[a,c]/; NonCommFreeQ[b];

holdDOT[a___,b1_ b2_,c___]:=
	b1 holdDOT[a,b2,c]/; NonCommFreeQ[b1] && !NonCommFreeQ[b2];

factorDirac[a_] :=
	a factorDirac[]/; NonCommFreeQ[a];

factorDirac[a_factorDirac b_.] :=
	a factorDirac[b];

factorDirac[a_SUNTrace b_.] :=
	a factorDirac[b];

factorDirac[a_PauliTrace b_.] :=
	a factorDirac[b];

factorDirac[a_ b_] :=
	a factorDirac[b]/; NonCommFreeQ[a] && !NonCommFreeQ[b];


factorPauli[a_] :=
	a factorPauli[]/; NonCommFreeQ[a];

factorPauli[a_factorPauli b_.] :=
	a factorPauli[b];

factorPauli[a_SUNTrace b_.] :=
	a factorPauli[b];

factorPauli[a_DiracTrace b_.] :=
	a factorPauli[b];

factorPauli[a_ b_] :=
	a factorPauli[b]/; NonCommFreeQ[a] && !NonCommFreeQ[b];

factorColor[a_] :=
	a factorColor[]/; NonCommFreeQ[a];

factorColor[a_factorColor b_.] :=
	a factorColor[b];

factorColor[a_PauliTrace b_.] :=
	a factorColor[b];

factorColor[a_DiracTrace b_.] :=
	a factorColor[b];

factorColor[a_ b_] :=
	a factorColor[b]/; NonCommFreeQ[a] && !NonCommFreeQ[b];

FCPrint[1,"FCTraceFactor.m loaded."];
End[]

