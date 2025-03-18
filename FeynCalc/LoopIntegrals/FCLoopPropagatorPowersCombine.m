(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopPropagatorPowersCombine									*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Combines propagator powers . 								*)

(* ------------------------------------------------------------------------ *)

FCLoopPropagatorPowersCombine::usage =
"FCLoopPropagatorPowersCombine[exp] combines the same propagators in a
FeynAmpDenominator to one propagator raised to an integer power.";

FCLoopPropagatorPowersCombine::failmsg =
"FCLoopPropagatorPowersCombine has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCLoopPropagatorPowersCombine`Private`"]

fdsor[a__] :=
	Apply[FeynAmpDenominator, Sort[{a}, FeynCalc`Package`lenso]];

Options[FCLoopPropagatorPowersCombine] = {
	FeynAmpDenominatorCombine	-> True,
	FCE 						-> False,
	FCI							-> False,
	FCVerbose					-> False
};

FCLoopPropagatorPowersCombine[exprRaw_/; FreeQ[exprRaw, OptionQ], OptionsPattern[]] :=
	Block[{	res, time, ppcVerbose, expr, ex, fadsList, momsList, momsListExpanded, repRule,
			fadsListEval},

		If [OptionValue[FCVerbose]===False,
				ppcVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					ppcVerbose=OptionValue[FCVerbose]
				];
		];

		If[	TrueQ[Head[exprRaw]=!=List],
			expr = {exprRaw},
			expr = exprRaw
		];

		FCPrint[1,"FCLoopPropagatorPowersCombine: Entering." , FCDoControl->ppcVerbose];
		FCPrint[3,"FCLoopPropagatorPowersCombine: Entering with: ", expr, FCDoControl->ppcVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"FCLoopPropagatorPowersCombine: Applying FCI." , FCDoControl->ppcVerbose];
		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];
		FCPrint[1,"FCLoopPropagatorPowersCombine: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->ppcVerbose];

		If[	FreeQ2[ex, {StandardPropagatorDenominator, CartesianPropagatorDenominator, GenericPropagatorDenominator}],
			If[	TrueQ[Head[exprRaw]=!=List],
				Return[First[ex]],
				Return[ex]
			];
		];


		(* FeynAmpDenominatorCombine is already efficient enough without parallelization *)
		If[	OptionValue[FeynAmpDenominatorCombine],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopPropagatorPowersCombine: Applying FeynAmpDenominatorCombine." , FCDoControl->ppcVerbose];
			ex= FeynAmpDenominatorCombine[ex, FCI->True];
			FCPrint[1,"FCLoopPropagatorPowersCombine: Done applying FeynAmpDenominatorCombine, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->ppcVerbose];
			FCPrint[3,"FCLoopPropagatorPowersCombine: After FeynAmpDenominatorCombine:: ", ex, FCDoControl->ppcVerbose];
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopPropagatorPowersCombine: Creating replacement rules.", FCDoControl->ppcVerbose];

		fadsList = Cases2[ex, FeynAmpDenominator];
		momsList 	= Cases2[fadsList, {Momentum, CartesianMomentum, TemporalMomentum}];
		momsListExpanded = MomentumExpand[momsList];
		repRule = Thread[Rule[momsList,momsListExpanded]];
		fadsListEval = fadsList /. Dispatch[repRule] /. FeynAmpDenominator -> fdsor;

		fadsListEval = fadsListEval /. FeynAmpDenominator -> fad //. {
			fad[a___,
				(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[x__, {n_Integer,s_}],
				(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[x__, {m_Integer,s_}],
			b___] :> fad[a,h[x,{n+m,s}],b]
		} /. (StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[__, {0,_}] :> Unevaluated[Sequence[]] /.
		fad[] -> 1 /. fad -> FeynAmpDenominator;

		repRule = Dispatch[Thread[Rule[fadsList,fadsListEval]]];
		FCPrint[1, "FCLoopPropagatorPowersCombine: Done creating replacement rules, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->ppcVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopPropagatorPowersCombine: Applying replacement rules.", FCDoControl->ppcVerbose];
		res = ex /. Dispatch[repRule];
		FCPrint[1, "FCLoopPropagatorPowersCombine: Done applying replacement rules, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->ppcVerbose];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		If[	TrueQ[Head[exprRaw]=!=List],
			res = First[res]
		];

		FCPrint[1,"FCLoopPropagatorPowersCombine: Leaving.", FCDoControl->ppcVerbose];
		FCPrint[3,"FCLoopPropagatorPowersCombine with: ", res, FCDoControl->ppcVerbose];

		res
	];

FCPrint[1,"FCLoopPropagatorPowersCombine.m loaded."];
End[]
