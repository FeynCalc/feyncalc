(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorSplit											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Splits FeynAmpDenominators into multiple parts					*)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorSplit::usage =
"FeynAmpDenominatorSplit[expr] splits all FeynAmpDenominator[a,b, ...] in expr
into FeynAmpDenominator[a]*FeynAmpDenominator[b]*... .
FeynAmpDenominatorSplit[expr,  Momentum ->q1] splits all FeynAmpDenominator in
expr into two products, one containing q1 and other momenta, the second being
free of q1.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynAmpDenominatorSplit`Private`"]

Options[FeynAmpDenominatorSplit] = {
	FCE				-> False,
	FCI				-> False,
	FCVerbose		-> False,
	List			-> False,
	Momentum 		-> All,
	MomentumExpand	-> True
};

FeynAmpDenominatorSplit[expr_, OptionsPattern[]] :=
	Block[{res,momList,fad,head, allFads, allFadsEval,repRule,optVerbose},

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		If[ !OptionValue[FCI],
			res = FCI[expr],
			res = expr
		];

		FCPrint[1, "FeynAmpDenominatorSplit: Entering.", FCDoControl->optVerbose];
		FCPrint[3, "FeynAmpDenominatorSplit Entering with: ", res, FCDoControl->optVerbose];


		If[	TrueQ[OptionValue[List]],
			head = List,
			head = Times
		];

		momList = OptionValue[Momentum];

		allFads = Cases2[res,FeynAmpDenominator];
		allFadsEval = allFads;

		FCPrint[1, "FeynAmpDenominatorSplit: Relevant denominators: ",allFadsEval, FCDoControl->optVerbose];

		If[	OptionValue[MomentumExpand],
			allFadsEval = MomentumExpand[allFadsEval];
			FCPrint[1, "FeynAmpDenominatorSplit: After MomentumExpand: ",allFadsEval, FCDoControl->optVerbose];
		];

		If[ momList=!=All && Head[momList]===List,
			allFadsEval = allFadsEval /. FeynAmpDenominator[props__] :> head[fad[SelectFree[{props},Sequence@@momList]],
				fad[SelectNotFree[{props},Sequence@@momList]]] /. fad[{}]:>1 /. fad[{pr__}]:>FeynAmpDenominator[pr],
			allFadsEval = allFadsEval /. FeynAmpDenominator[a__] :> head@@Map[FeynAmpDenominator, {a}]
		];

		FCPrint[1, "FeynAmpDenominatorSplit: After splitting: ",allFadsEval, FCDoControl->optVerbose];

		repRule = Thread[Rule[allFads,allFadsEval]];
		res = res/. Dispatch[repRule];

		If [OptionValue[FCE],
			res=FCE[res]
		];

		FCPrint[1, "FeynAmpDenominatorSplit: Leaving.", FCDoControl->optVerbose];
		FCPrint[3, "FFeynAmpDenominatorSplit Leaving with: ", res, FCDoControl->optVerbose];

		res
	];

FCPrint[1,"FeynAmpDenominatorSplit.m loaded."];
End[]
