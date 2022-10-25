(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Explicit															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary: Explicit form of operators										*)

(* ------------------------------------------------------------------------ *)

Explicit::usage =
"Explicit is an option for FieldStrength, GluonVertex, SUNF, and
Twist2GluonOperator. If set to True the full form of the operator is inserted.

Explicit[exp] inserts explicit expressions of GluonVertex, Twist2GluonOperator
etc. in exp. SUNFs are replaced by SUNTrace objects.";

(* ------------------------------------------------------------------------ *)


Begin["`Package`"];

SymbolsWithExplicitOption;

SymbolsWithExplicitOption[optsExplicit___]:= {

	FeynCalc`SUNF[xx__, op:OptionsPattern[]]  :>
		FeynCalc`SUNF[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[SUNF]], op],

	FeynCalc`SUND[xx__, op:OptionsPattern[]]  :>
		FeynCalc`SUND[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[SUND]], op],

	FeynCalc`SUNTrace[xx__, op:OptionsPattern[]]  :>
		FeynCalc`SUNTrace[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[SUNTrace]], op],

	FeynCalc`GluonVertex[xx__, op:OptionsPattern[]]  :>
		FeynCalc`GluonVertex[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[GluonVertex]], op],

	FeynCalc`GluonPropagator[xx__, op : OptionsPattern[]] :>
		FeynCalc`GluonPropagator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[GluonPropagator]], op],

	FeynCalc`GhostPropagator[xx__, op : OptionsPattern[]] :>
		FeynCalc`GhostPropagator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[GhostPropagator]], op],

	FeynCalc`QuarkPropagator[xx__, op : OptionsPattern[]] :>
		FeynCalc`QuarkPropagator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[QuarkPropagator]], op],

	FeynCalc`GluonGhostVertex[xx__, op : OptionsPattern[]] :>
		FeynCalc`GluonGhostVertex[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[GluonGhostVertex]], op],

	FeynCalc`QuarkGluonVertex[xx__, op : OptionsPattern[]] :>
		FeynCalc`QuarkGluonVertex[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[QuarkGluonVertex]], op],

	FeynCalc`Twist2GluonOperator[xx__, op : OptionsPattern[]] :>
		FeynCalc`Twist2GluonOperator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[Twist2GluonOperator]], op],

	FeynCalc`Twist2QuarkOperator[xx__, op : OptionsPattern[]] :>
		FeynCalc`Twist2QuarkOperator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[Twist2QuarkOperator]], op],

	FeynCalc`FieldStrength[xx__, op : OptionsPattern[]] :>
		FeynCalc`FieldStrength[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[FieldStrength]], op],

	FeynCalc`FCChargeConjugateTransposed[xx_, op:OptionsPattern[]]  :>
		FeynCalc`FCChargeConjugateTransposed[xx, Explicit->True, Sequence@@FilterRules[{op}, Except[Explicit]]],

	FeynCalc`FCClausen[xx_, yy_, op:OptionsPattern[]]  :>
		FeynCalc`FCClausen[xx, yy, Explicit->True,	Sequence@@FilterRules[{op}, Except[Explicit]]]
};

End[]

Begin["`Explicit`Private`"];

exVerbose::usage="";

Options[Explicit] = {
	CouplingConstant	-> SMP["g_s"],
	Dimension 			-> D,
	ExpandScalarProduct	-> True,
	FCE					-> False,
	FCI					-> False,
	FCVerbose			-> False,
	Gauge 				-> 1,
	OPE 				-> False,
	SUND				-> False,
	SUNF				-> False,
	SUNTrace			-> False
};

Explicit[expr_, opts:OptionsPattern[]] :=
	Block[{heads, ex, res, rule, list, listEval},

		If [OptionValue[FCVerbose]===False,
			exVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				exVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"Explicit: Entering.", FCDoControl->exVerbose];
		FCPrint[3,"Explicit: Entering with ", expr, FCDoControl->exVerbose];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		heads = Head /@ First /@ FeynCalc`Package`SymbolsWithExplicitOption[];

		If[	!OptionValue[SUND],
			heads = SelectFree[heads,SUND]
		];

		If[	!OptionValue[SUNF],
			heads = SelectFree[heads,SUNF]
		];

		If[	!OptionValue[SUNTrace],
			heads = SelectFree[heads,SUNTrace]
		];

		list = Cases2[ex,heads];

		FCPrint[3,"Explicit: Relevant symbols in the expression: ", list, FCDoControl->exVerbose];

		listEval = list /. FeynCalc`Package`SymbolsWithExplicitOption[opts];

		FCPrint[3,"Explicit: Explicit form of the relevant symbols: ", listEval, FCDoControl->exVerbose];

		If[	OptionValue[ExpandScalarProduct],
			listEval = ExpandScalarProduct[listEval]
		];

		rule = Thread[Rule[list,listEval]];
		res = ex/.Dispatch[rule];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"Explicit: Leaving.", FCDoControl->exVerbose];
		FCPrint[3,"Explicit: Leaving with ", res, FCDoControl->exVerbose];


		res
	];

FCPrint[1,"Explicit.m loaded"];
End[]
