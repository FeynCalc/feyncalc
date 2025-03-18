(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorCombine										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary: Combines products of FeynAmpDenominators.						*)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorCombine::usage =
"FeynAmpDenominatorCombine[expr] expands expr with respect to
FeynAmpDenominator and combines products of FeynAmpDenominator in expr into
one FeynAmpDenominator.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

End[]

Begin["`FeynAmpDenominatorCombine`Private`"]

feyncomb[] =
	1;

feyncomb /:
	feyncomb[a__] feyncomb[b__] :=
		feyncomb[a, b];

feyncomb /:
	feyncomb[a__]^n_Integer?Positive :=
		feyncomb @@ Flatten[Table[{a}, {n}]];

fdsor[a__] :=
	Apply[FeynAmpDenominator, Sort[{a}, FeynCalc`Package`lenso]];

Options[FeynAmpDenominatorCombine] = {
	FCE 			-> False,
	FCI 			-> False,
	FCVerbose		-> False,
	Momentum		-> All
};

FeynAmpDenominatorCombine[expr_, OptionsPattern[]] :=
	Block[{	ex, res, optMomentum, fadcVerbose, time,
			momsList, momsListExpanded, repRule, feyncombList, feyncombListEval},

		If[	OptionValue[FCVerbose]===False,
			fadcVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fadcVerbose=OptionValue[FCVerbose]
			];
		];

		time=AbsoluteTime[];

		FCPrint[1,"FeynAmpDenominatorCombine: Applying FCI.", FCDoControl->fadcVerbose];
		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];
		FCPrint[1, "FeynAmpDenominatorCombine: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fadcVerbose];

		optMomentum 		= OptionValue[Momentum];

		If[FreeQ[ex,FeynAmpDenominator],
			Return[ex]
		];

		If[Head[expr]=!=List,
			ex = {ex}
		];



		time=AbsoluteTime[];
		FCPrint[1,"FeynAmpDenominatorCombine: Applying Expand2.", FCDoControl->fadcVerbose];
		res = Expand2[ex,FeynAmpDenominator];
		FCPrint[1, "FeynAmpDenominatorCombine: Done applying Expand2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fadcVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"FeynAmpDenominatorCombine: Precombining FeynAmpDenominators.", FCDoControl->fadcVerbose];
		If[ optMomentum=!=All && Head[optMomentum]===List,
			res = res /. FeynAmpDenominator[x__]/;!FreeQ2[{x},optMomentum] :> feyncomb[x],
			res = res /. FeynAmpDenominator -> feyncomb
		];
		FCPrint[1, "FeynAmpDenominatorCombine: Done precombining FeynAmpDenominators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fadcVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FeynAmpDenominatorCombine: Creating replacement rules.", FCDoControl->fadcVerbose];
		feyncombList = Cases2[res,feyncomb];
		momsList 	= Cases2[feyncombList, {Momentum, CartesianMomentum, TemporalMomentum}];
		momsListExpanded = MomentumExpand[momsList];
		repRule = Thread[Rule[momsList,momsListExpanded]];
		feyncombListEval = feyncombList /. Dispatch[repRule] /. feyncomb -> fdsor;
		repRule = Dispatch[Thread[Rule[feyncombList,feyncombListEval]]];
		FCPrint[1, "FeynAmpDenominatorCombine: Done creating replacement rules, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fadcVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"FeynAmpDenominatorCombine: Applying replacement rules .", FCDoControl->fadcVerbose];
		res = res /. Dispatch[repRule];

		FCPrint[1, "FeynAmpDenominatorCombine: Done applying replacement rules, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fadcVerbose];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		If[Head[expr]=!=List,
			res = First[res]
		];

		FCPrint[1,"FeynAmpDenominatorCombine: Leaving.", FCDoControl->fadcVerbose];
		FCPrint[3,"FeynAmpDenominatorCombine: Leaving with: ", res, FCDoControl->fadcVerbose];

		res


	];

FCPrint[1,"FeynAmpDenominatorCombine.m loaded."];
End[]
