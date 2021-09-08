(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopRemoveNegativePropagatorPowers										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Expands propagator powers . 								*)

(* ------------------------------------------------------------------------ *)

FCLoopRemoveNegativePropagatorPowers::usage =
"FCLoopRemoveNegativePropagatorPowers[exp] rewrites propagators raised to
integer powers as products.";

FCLoopRemoveNegativePropagatorPowers::failmsg =
"FCLoopRemoveNegativePropagatorPowers has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCLoopRemoveNegativePropagatorPowers`Private`"]

Options[FCLoopRemoveNegativePropagatorPowers] = {
	FCE 							-> False,
	FCI 							-> False,
	FCLoopPropagatorPowersCombine	-> True
};

FCLoopRemoveNegativePropagatorPowers[expr_, OptionsPattern[]] :=
	Block[{ex, res, fadsList, fadsListEval, repRule, fad},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	OptionValue[FCLoopPropagatorPowersCombine],
			ex = FCLoopPropagatorPowersCombine[ex,FCI->True]
		];

		If[FreeQ2[ex, {StandardPropagatorDenominator, CartesianPropagatorDenominator, GenericPropagatorDenominator}],
			Return[ex]
		];

		fadsList = Cases2[ex, FeynAmpDenominator];

		fadsListEval = fadsList /. FeynAmpDenominator -> fad /. {
			fad[a___,
				(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[x__, {n_Integer,s_}], b___]/; n<0 :>
			FeynAmpDenominatorExplicit[FeynAmpDenominator[h[x,{n,s}]],FCI->True] fad[a,b]
		} /. fad[] -> 1 /. fad -> FeynAmpDenominator;

		repRule = Thread[Rule[fadsList,fadsListEval]];

		res = ex /. Dispatch[repRule];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res


	];






FCPrint[1,"FCLoopRemoveNegativePropagatorPowers.m loaded."];
End[]
