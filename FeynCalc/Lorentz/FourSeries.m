(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FourSeries													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:	Compute series expansion in the given 4-vector

				Supports parallel evaluation [X]
*)

(* ------------------------------------------------------------------------ *)

FourSeries::usage =
"FourSeries[exp, {p,p0,n}] calculates Taylor series of exp w.r.t the $4$-vector
$p$ to $n$th order.
If the expression diverges at $p = p_0$, it will be returned unevaluated.";

FourSeries::failmsg =
"Error! FourSeries has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FourSeries`Private`"]


Options[FourSeries] = {
	Check				-> True,
	Collecting 			-> True,
	Dimension			-> D,
	EpsEvaluate 		-> True,
	ExpandScalarProduct	-> True,
	FCE 				-> False,
	FCI 				-> False,
	FCParallelize		-> False,
	FCVerbose 			-> False,
	Factoring 			-> {Factor, 5000},
	Head				-> Identity,
	TimeConstrained 	-> 3,
	Last				-> 1
};

FourSeries[expr_List, {q_,q0_,n_Integer},opts:OptionsPattern[]] :=
	Block[{optVerbose, res, time},

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];
		time = AbsoluteTime[];

		FCPrint[1, "FourSeries: Entering.", FCDoControl->optVerbose];

		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
			FCPrint[1,"FourSeries: Applying FourSeries in parallel.", FCDoControl->optVerbose];

			res = ParallelMap[FourSeries[#, {q,q0,n}, FilterRules[{opts}, Except[FCParallelize | FCVerbose]]]&,expr,
			DistributedContexts -> None, Method->"ItemsPerEvaluation" -> Ceiling[N[Length[expr]/$KernelCount]/10]];
			FCPrint[1, "FourSeries: Done applying FourSeries in parallel, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],

			FCPrint[1,"FourSeries: Applying FourSeries.", FCDoControl->optVerbose];
			res = Map[FourSeries[#, {q,q0,n}, FilterRules[{opts}, Except[FCParallelize | FCVerbose]]]&,expr];
			FCPrint[1, "FourSeries: Done applying FourSeries, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose]
		];

		FCPrint[1, "FourSeries: Leaving.", FCDoControl->optVerbose];

		res
	];



FourSeries[expr_/;Head[expr]=!=List, {q_,q0_,n_Integer}, OptionsPattern[]] :=
	Block[{	ex, time, res, tmp, optVerbose, optDimension, optHead,
			optLast, flag, check},

		optDimension 	= OptionValue[Dimension];
		optHead			= OptionValue[Head];
		optLast			= OptionValue[Last];

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FourSeries: Entering.", FCDoControl->optVerbose];
		FCPrint[3, "FourSeries: Entering with: ", expr, FCDoControl->optVerbose];
		FCPrint[3, "FourSeries: Series expansion around ", {q,q0,n}, FCDoControl->optVerbose];


		If [!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		FCPrint[3, "FourSeries: Entering with: ", expr, FCDoControl->optVerbose];
		ex = MomentumExpand[ex,Momentum->{q}];

		If[	FreeQ[ex,Momentum[q,optDimension]],
			FCPrint[1, "FourSeries: Nothing to do.", FCDoControl->optVerbose];
			Return[ex]
		];

		If[	OptionValue[EpsEvaluate],
			ex= EpsEvaluate[ex,FCI->True]
		];

		If[	OptionValue[Check],
			(*Todo HelpFlag*)
			check = FeynAmpDenominatorExplicit[ex,FCI->True]//ExpandScalarProduct[#,FCI->True]&//Denominator;
			If[	PossibleZeroQ[check/.q->q0],
				Return[ex]
			]
		];

		res = ex/.q->q0;

		If[n>0,
			FCPrint[1, "FourSeries: Applying calcExpOrder.", FCDoControl->optVerbose];
			time=AbsoluteTime[];
			tmp=Table[flag[i]*calcExpOrder[ex,q,q0,i,optDimension,OptionValue[Head]],{i,1,n}];
			tmp = tmp /. flag[n]->optLast /. flag[_]->1;
			res = res + Total[tmp];
			FCPrint[1, "FourSeries: calcExpOrder done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
			FCPrint[3, "FourSeries: After calcExpOrder: ", tmp, FCDoControl->optVerbose]
		];

		If[	OptionValue[ExpandScalarProduct],
			res = ExpandScalarProduct[res,FCI->True]
		];

		If[	OptionValue[Collecting],
			FCPrint[1, "FourSeries: Applying Collect2.", FCDoControl->optVerbose];
			time=AbsoluteTime[];
			res = Collect2[res,q,Factoring->OptionValue[Factoring],TimeConstrained->OptionValue[TimeConstrained]];
			FCPrint[1, "FourSeries: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
			FCPrint[3, "FourSeries: After Collect2: ", res, FCDoControl->optVerbose]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FourSeries: Leaving.", FCDoControl->optVerbose];
		FCPrint[3, "FourSeries: Leaving with: ", res, FCDoControl->optVerbose];


		res
	];

calcExpOrder[ex_,q_,q0_,n_,dim_,head_]:=
	Block[{fvs},
		fvs=Table[Pair[Momentum[head[q-q0], dim], LorentzIndex[Unique[], dim]],{i,1,n}];
		(1/n!)*Contract[Times@@fvs*(FourDivergence[ex, Sequence@@(fvs/.q0->0/.head->Identity),Contract -> False]/.q->q0),
			FCI->True]
	];




FCPrint[1,"FourSeries.m loaded."];
End[]
