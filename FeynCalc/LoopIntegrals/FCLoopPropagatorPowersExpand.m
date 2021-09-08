(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopPropagatorPowersExpand										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Expands propagator powers . 								*)

(* ------------------------------------------------------------------------ *)

FCLoopPropagatorPowersExpand::usage =
"FCLoopPropagatorPowersExpand[exp] rewrites propagators raised to integer
powers as products.";

FCLoopPropagatorPowersExpand::failmsg =
"FCLoopPropagatorPowersExpand has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCLoopPropagatorPowersExpand`Private`"]

Options[FCLoopPropagatorPowersExpand] = {
	FCE -> False,
	FCI -> False
};

FCLoopPropagatorPowersExpand[expr_, OptionsPattern[]] :=
	Block[{ex, res, fadsList, fadsListEval, repRule},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[FreeQ2[ex, {StandardPropagatorDenominator, CartesianPropagatorDenominator, GenericPropagatorDenominator}],
			Return[ex]
		];

		fadsList = Cases2[ex, FeynAmpDenominator];

		fadsListEval = fadsList /. {
			(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[x__, {n_Integer,s_}]/;Abs[n]>1 :>
				Sequence@@ConstantArray[h[x,{1,s}],n]
		};

		repRule = Thread[Rule[fadsList,fadsListEval]];

		res = ex /. Dispatch[repRule];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res


	];






FCPrint[1,"FCLoopPropagatorPowersExpand.m loaded."];
End[]
