(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopPropagatorPowersCombine									*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
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

Options[FCLoopPropagatorPowersCombine] = {
	FCE -> False,
	FCI -> False
};

FCLoopPropagatorPowersCombine[expr_, OptionsPattern[]] :=
	Block[{ex, res, fadsList, fadsListEval, repRule, fad},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[FreeQ2[ex, {StandardPropagatorDenominator, CartesianPropagatorDenominator, GenericPropagatorDenominator}],
			Return[ex]
		];

		fadsList = Cases2[ex, FeynAmpDenominator];

		fadsListEval = fadsList /. FeynAmpDenominator -> FeynCalc`Package`fdsor /. FeynAmpDenominator -> fad //. {
			fad[a___,
				(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[x__, {n1_Integer,s_}],
				(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[x__, {n2_Integer,s_}],
			b___] :>
			fad[a,h[x,{n1+n2,s}],b]
		} /. (StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[__, {0,_}] :> Unevaluated[Sequence[]] /.
		fad[] -> 1 /. fad -> FeynAmpDenominator;

		repRule = Thread[Rule[fadsList,fadsListEval]];

		res = ex /. Dispatch[repRule];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res


	];


FCPrint[1,"FCLoopPropagatorPowersCombine.m loaded."];
End[]
