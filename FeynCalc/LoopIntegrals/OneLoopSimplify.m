(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OneLoopSimpify													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Simplification of 1-loop integrals						    *)

(* ------------------------------------------------------------------------ *)

OneLoopSimplify::usage =
"OneLoopSimplify[amp, q] simplifies the one-loop amplitude amp. The second
argument denotes the integration momentum.

If the first argument has head FeynAmp then OneLoopSimplify[FeynAmp[name, k,
expr], k] transforms to OneLoopSimplify[expr, k]";

OneLoopSimplify::nivar =
"The integration variable is not found in the integrand. \
Please check that the name of the second argument is correct.";

OneLoopSimplify::numerators =
"OneLoopSimplify failed to eliminate all loop-momentum dependent numerators in \
then give loop integral(s). Please examine the output carfeully.";

OneLoopSimplify::failmsg =
"Error! OneLoopSimplify has encountered a fatal \
problem and must abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`OneLoopSimplify`Private`"]

olsVerbose::usage="";

Options[OneLoopSimplify] = {
	Collecting 					-> True,
	Dimension 					-> D,
	DiracSimplify 				-> True,
	DiracSpinorNormalization	-> "Relativistic",
	ExpandScalarProduct 		-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	Factoring 					-> Automatic,
	FinalSubstitutions			-> {},
	OPE1Loop					-> False,
	PowerSimplify				-> True,
	SpinorChainEvaluate 		-> True,
	SUNNToCACF					-> True,
	SUNTrace					-> False,
	ToPaVe						-> False,
	UsePaVeBasis				-> False
};

(*Do we really need to support this syntax???*)
OneLoopSimplify[qu_ /; Head[qu]=== Symbol, amp_ /;Head[amp]=!=Symbol, opt:OptionsPattern[]] :=
	OneLoopSimplify[amp,qu,opt] /; !FreeQ[amp,qu];

OneLoopSimplify[FeynAmp[_, qu_Symbol, expr_], ___, opt:OptionsPattern[]] :=
	OneLoopSimplify[expr, qu, opt];

OneLoopSimplify[expr_, qu_, OptionsPattern[]] :=
	Block[ {amp, q, dim, sunntocacf, substis,optDiracSimplify,
	ope1loop, time, optCollecting, suntrace,tmp, optFactoring, res},

		If [!FreeQ[$ScalarProducts, q],
			Message[OneLoopSimplify::failmsg, "The loop momentum " <> ToString[qu,InputForm] <> " has scalar product rules attached to it."];
			Abort[]
		];

		If[	$KeepLogDivergentScalelessIntegrals && OptionValue[OPE1Loop],
			Message[OneLoopSimplify::failmsg, "OneLoopSimplify does not support the option $KeepLogDivergentScalelessIntegrals withe OPE1Loop set to true."];
			Abort[]
		];

		If[	!FreeQ2[{expr}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		If [OptionValue[FCVerbose]===False,
			olsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				olsVerbose=OptionValue[FCVerbose]
			];
		];

		If[ FreeQ[expr, qu],
			Print[expr];
			Print[qu];
			Message[OneLoopSimplify::nivar, qu];
			Return[expr]
		];

		(* 	Notice that here we apply ChangeDimension only to the isolated tensor integrals,
			not to the whole expression! If the dimension of the whole expression must be changed,
			then this should be explicitly done by the user! *)

		q = qu;
		dim					= OptionValue[Dimension];
		optDiracSimplify	= OptionValue[DiracSimplify];
		sunntocacf 			= OptionValue[SUNNToCACF];
		suntrace 			= OptionValue[SUNTrace];
		ope1loop 			= OptionValue[OPE1Loop];
		substis 			= OptionValue[FinalSubstitutions];
		optCollecting	 	= OptionValue[Collecting];


		If[ OptionValue[Factoring] === Automatic,
			optFactoring =
				Function[x, If[ LeafCount[x] <  5000,
								Factor2[x],
								x
							]
				],
			optFactoring = OptionValue[Factoring]
		];



		FCPrint[1,"OneLoopSimplify: Entering.", FCDoControl->olsVerbose];
		FCPrint[3,"OneLoopSimplify: Entering with: ", expr, FCDoControl->olsVerbose];

		If[	OptionValue[FCI],
			amp = expr,
			amp = FCI[expr]
		];


		time=AbsoluteTime[];
		FCPrint[1, "OneLoopSimplify: Applying Contract.", FCDoControl->olsVerbose];
		tmp = Contract[amp, EpsContract -> False, Expanding -> False, FCI -> True];
		FCPrint[1, "OneLoopSimplify: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
		FCPrint[3, "OneLoopSimplify: After Contract: ", tmp, FCDoControl->olsVerbose];

		If[ !FreeQ2[tmp, FeynCalc`Package`SUNHeadsList],
			time=AbsoluteTime[];
			FCPrint[1, "OneLoopSimplify: Applying SUNSimplify.", FCDoControl->olsVerbose];
			tmp = SUNSimplify[tmp, SUNNToCACF -> sunntocacf, SUNTrace -> suntrace, Explicit -> False];
			FCPrint[1, "OneLoopSimplify: SUNSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
			FCPrint[3, "OneLoopSimplify: After SUNSimplify: ", tmp, FCDoControl->olsVerbose]
		];

		If[	!FreeQ2[tmp, FeynCalc`Package`DiracHeadsList] && optDiracSimplify,
			time=AbsoluteTime[];
			FCPrint[1, "OneLoopSimplify: Applying DiracSimplify.", FCDoControl->olsVerbose];
			tmp = DiracSimplify[tmp, FCI->True, SpinorChainEvaluate -> OptionValue[SpinorChainEvaluate],
				DiracSpinorNormalization -> OptionValue[DiracSpinorNormalization]];
			FCPrint[1, "OneLoopSimplify: DiracSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
			FCPrint[3, "OneLoopSimplify: After DiracSimplify: ", tmp, FCDoControl->olsVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "OneLoopSimplify: Applying Contract.", FCDoControl->olsVerbose];
		tmp = Contract[tmp, EpsContract -> False, Expanding -> False, FCI -> True];
		FCPrint[1, "OneLoopSimplify: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
		FCPrint[3, "OneLoopSimplify: After Contract: ", tmp, FCDoControl->olsVerbose];



		If[ (!FreeQ[tmp, OPEDelta]) && ope1loop,
			time=AbsoluteTime[];
			FCPrint[1, "OneLoopSimplify: Applying OPE1Loop.", FCDoControl->olsVerbose];
			tmp = OPE1Loop[q, tmp, SUNNToCACF -> sunntocacf, Explicit -> False];
			FCPrint[1, "OneLoopSimplify: OPE1Loop done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
			FCPrint[3, "OneLoopSimplify: After OPE1Loop: ", tmp, FCDoControl->olsVerbose]
		];


		time=AbsoluteTime[];
		FCPrint[1, "OneLoopSimplify: Applying TID.", FCDoControl->olsVerbose];
		tmp =  TID[tmp,  q, Dimension -> dim, FCI->True, DiracSimplify -> optDiracSimplify, ToPaVe->OptionValue[ToPaVe], UsePaVeBasis->OptionValue[UsePaVeBasis],
				SpinorChainEvaluate -> OptionValue[SpinorChainEvaluate], DiracSpinorNormalization -> OptionValue[DiracSpinorNormalization]];
		FCPrint[1, "OneLoopSimplify: TID done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
		FCPrint[3, "OneLoopSimplify: After TID: ", tmp, FCDoControl->olsVerbose];



		tmp = tmp /. substis;


		If[	!FreeQ2[tmp, FeynCalc`Package`DiracHeadsList] && optDiracSimplify,
			time=AbsoluteTime[];
			FCPrint[1, "OneLoopSimplify: Applying DiracSimplify.", FCDoControl->olsVerbose];
			tmp = DiracSimplify[tmp, FCI->True, SpinorChainEvaluate -> OptionValue[SpinorChainEvaluate],
				DiracSpinorNormalization -> OptionValue[DiracSpinorNormalization]];
			FCPrint[1, "OneLoopSimplify: DiracSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
			FCPrint[3, "OneLoopSimplify: After DiracSimplify: ", tmp, FCDoControl->olsVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "OneLoopSimplify: Applying Contract.", FCDoControl->olsVerbose];
		tmp = Contract[tmp, EpsContract -> False, Expanding -> False, FCI -> True];
		FCPrint[1, "OneLoopSimplify: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
		FCPrint[3, "OneLoopSimplify: After Contract: ", tmp, FCDoControl->olsVerbose];


		If[	OptionValue[ExpandScalarProduct],
			time=AbsoluteTime[];
			FCPrint[1, "OneLoopSimplify: Applying ExpandScalarProduct.", FCDoControl->olsVerbose];
			tmp = ExpandScalarProduct[tmp, FCI -> True];
			FCPrint[1, "OneLoopSimplify: ExpandScalarProduct done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
			FCPrint[3, "OneLoopSimplify: After ExpandScalarProduct: ", tmp, FCDoControl->olsVerbose];
		];


		If[	OptionValue[PowerSimplify],
			time=AbsoluteTime[];
			FCPrint[1, "OneLoopSimplify: Applying PowerSimplify.", FCDoControl->olsVerbose];
			tmp = PowerSimplify[tmp];
			FCPrint[1, "OneLoopSimplify: PowerSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
			FCPrint[3, "OneLoopSimplify: After PowerSimplify: ", tmp, FCDoControl->olsVerbose];
		];

		time=AbsoluteTime[];
		FCPrint[1, "OneLoopSimplify: Applying FeynAmpDenominatorSimplify.", FCDoControl->olsVerbose];
		tmp = FDS[tmp, q, FCI -> True];
		FCPrint[1, "OneLoopSimplify: FeynAmpDenominatorSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->olsVerbose];
		FCPrint[3, "OneLoopSimplify: After FeynAmpDenominatorSimplify: ", tmp, FCDoControl->olsVerbose];

		If[ OptionValue[Collecting],
			tmp = Collect2[tmp, Join[{qu},FeynCalc`Package`PaVeHeadsList], Expanding->False, Factoring->optFactoring]
		];

		res = tmp;

		If [OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"OneLoopSimplify: Leaving.", FCDoControl->olsVerbose];
		FCPrint[3,"OneLoopSimplify: Leaving with: ", res, FCDoControl->olsVerbose];

		res
	];


FCPrint[1,"OneLoopSimplify.m loaded."];
End[]
