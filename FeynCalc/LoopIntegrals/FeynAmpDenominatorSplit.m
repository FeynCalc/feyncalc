(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorSplit											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary: Splits FeynAmpDenominators into multiple parts					*)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorSplit::usage =
"FeynAmpDenominatorSplit[expr] splits all FeynAmpDenominator[a,b, ...] \
into FeynAmpDenominator[a]*FeynAmpDenominator[b] ... . \n
FeynAmpDenominatorSplit[expr, Momentum->{q1,q2,q3,...}] splits every FeynAmpDenominator \
in expr into a product of two, one containing q1,q2,q3,... and other momenta, \
the second without those momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynAmpDenominatorSplit`Private`"]

Options[FeynAmpDenominatorSplit] = {
	FCE				-> False,
	FCI				-> False,
	List			-> False,
	Momentum 		-> All,
	MomentumExpand	-> True
};

FeynAmpDenominatorSplit[expr_, OptionsPattern[]] :=
	Block[{res,momList,fad,head, allFads, allFadsEval,repRule},

		If[ !OptionValue[FCI],
			res = FCI[expr],
			res = expr
		];
		If[	TrueQ[OptionValue[List]],
			head = List,
			head = Times
		];

		momList = OptionValue[Momentum];

		allFads = Cases2[res,FeynAmpDenominator];
		allFadsEval = allFads;


		If[	OptionValue[MomentumExpand],
			allFadsEval = MomentumExpand[allFadsEval]
		];

		If[ momList=!=All && Head[momList]===List,
			allFadsEval = allFadsEval /. FeynAmpDenominator[props__] :> head[fad[SelectFree[{props},Sequence@@momList]],
				fad[SelectNotFree[{props},Sequence@@momList]]] /. fad[{}]:>1 /. fad[{pr__}]:>FeynAmpDenominator[pr],
			allFadsEval = allFadsEval /. FeynAmpDenominator[a__] :> head@@Map[FeynAmpDenominator, {a}]
		];

		repRule = Thread[Rule[allFads,allFadsEval]];
		res = res/. Dispatch[repRule];

		If [OptionValue[FCE],
			res=FCE[res]
		];

		res
	];

FCPrint[1,"FeynAmpDenominatorSplit.m loaded."];
End[]
