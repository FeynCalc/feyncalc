(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TR														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Dirac trace calculation										*)

(* ------------------------------------------------------------------------ *)

TR::usage=
"TR[exp] calculates the Dirac trace of exp.

If the option SUNSimplify is set to True (default), $SU(N)$ algebra is
simplified as well.

Notice that TR is a legacy function that should not be used in new codes.
Instead, you can wrap your string Dirac matrices with DiracTrace and
subsequently apply DiracSimplify to calculate the trace.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TR`Private`"]

optVerbose::usage="";

Options[TR] = {
	Contract			-> True,
	DiracTraceEvaluate	-> True,
	EpsContract			-> True,
	Expand				-> True,
	Explicit			-> True,
	FCI					-> False,
	FCE					-> False,
	FCVerbose			-> True,
	Factoring			-> Automatic,
	Mandelstam			-> {},
	PairCollect			-> False,
	SUNSimplify			-> True,
	SUNNToCACF			-> True,
	TraceOfOne			-> 4,
	West				-> True
};


TR[expr_, rul:OptionsPattern[]] :=
	Block[{	ex, time, optSUNNToCACF, diracTraceOpts, trOpts, optVerbose},

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"TR: Entering.", FCDoControl->optVerbose];
		FCPrint[3,"TR: Entering with: ", expr, FCDoControl->optVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		optSUNNToCACF			= OptionValue[SUNNToCACF];
		trOpts 					= Flatten[Join[{rul}, FilterRules[Options[TR], Except[{rul}]]]];
		diracTraceOpts			= Flatten[FilterRules[Options[trOpts], Options[DiracTrace]]];

		If[	OptionValue[Explicit],
			ex = Explicit[ex]
		];

		time=AbsoluteTime[];
		FCPrint[1,"TR: Computing the overall Dirac trace.", FCDoControl->optVerbose];
		ex = DiracTrace[ex, FCI->True, diracTraceOpts];
		FCPrint[1,"TR: Done computing the overall Dirac trace, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"TR: Applying SUNSimplify.", FCDoControl->optVerbose];
		ex = SUNSimplify[ex, FCI->True, SUNNToCACF -> optSUNNToCACF];
		FCPrint[1,"TR: Done applying SUNSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		If[	OptionValue[FCE],
			ex = FCE[ex]
		];

		FCPrint[1,"TR: Leaving.", FCDoControl->optVerbose];
		FCPrint[3,"TR: Leaving with: ", ex, FCDoControl->optVerbose];
		ex
	];

FCPrint[1,"TR.m loaded"];
End[]
