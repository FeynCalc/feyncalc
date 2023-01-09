(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Explicit															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary: Explicit form of operators										*)

(* ------------------------------------------------------------------------ *)

Explicit::usage =
"Explicit[exp] inserts explicit expressions of GluonVertex,
Twist2GluonOperator, SUNF etc. in exp.

To rewrite the $SU(N)$ structure constants in terms of traces, please set the
corresponding options SUNF or SUND to True.

The color traces are left untouched unless the option SUNTrace is set to True.
In this case they will be rewritten in terms of structure constants.

Explicit is also an option for FieldStrength, GluonVertex, SUNF,
Twist2GluonOperator etc. If set to True the full form of the operator is
inserted.";

(* ------------------------------------------------------------------------ *)


Begin["`Package`"];

RulesForSymbolsWithExplicitOption;



End[]

Begin["`Explicit`Private`"];

exVerbose::usage="";


RulesForSymbolsWithExplicitOption = {
	BuiltInSymbolsWithExplicitOption
};

BuiltInSymbolsWithExplicitOption[optsExplicit___]:= {

	FCTripleProduct[xx__, op:OptionsPattern[]]  :>
		FCTripleProduct[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[FCTripleProduct]], op],

	SUNF[xx__, op:OptionsPattern[]]  :>
		SUNF[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[SUNF]], op],

	SUND[xx__, op:OptionsPattern[]]  :>
		SUND[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[SUND]], op],

	GluonVertex[xx__, op:OptionsPattern[]]  :>
		GluonVertex[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[GluonVertex]], op],

	GluonPropagator[xx__, op : OptionsPattern[]] :>
		GluonPropagator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[GluonPropagator]], op],

	GhostPropagator[xx__, op : OptionsPattern[]] :>
		GhostPropagator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[GhostPropagator]], op],

	QuarkPropagator[xx__, op : OptionsPattern[]] :>
		QuarkPropagator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[QuarkPropagator]], op],

	GluonGhostVertex[xx__, op : OptionsPattern[]] :>
		GluonGhostVertex[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[GluonGhostVertex]], op],

	QuarkGluonVertex[xx__, op : OptionsPattern[]] :>
		QuarkGluonVertex[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[QuarkGluonVertex]], op],

	Twist2GluonOperator[xx__, op : OptionsPattern[]] :>
		Twist2GluonOperator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[Twist2GluonOperator]], op],

	Twist2QuarkOperator[xx__, op : OptionsPattern[]] :>
		Twist2QuarkOperator[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[Twist2QuarkOperator]], op],

	FieldStrength[xx__, op : OptionsPattern[]] :>
		FieldStrength[xx, Explicit -> True, Sequence@@FilterRules[{optsExplicit}, Options[FieldStrength]], op],

	FCChargeConjugateTransposed[xx_, op:OptionsPattern[]]  :>
		FCChargeConjugateTransposed[xx, Explicit->True, Sequence@@FilterRules[{op}, Except[Explicit]]],

	FCClausen[xx_, yy_, op:OptionsPattern[]]  :>
		FCClausen[xx, yy, Explicit->True,	Sequence@@FilterRules[{op}, Except[Explicit]]]
};

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
	SUNF				-> False
};

Explicit[expr_, opts:OptionsPattern[]] :=
	Block[{heads, ex, res, rule, list, listEval, repRules},

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

		heads = Head/@Union[Flatten[Map[Function[x, {First /@ x[{}]}], RulesForSymbolsWithExplicitOption]]];

		FCPrint[3,"Explicit: Relevant heads: ", heads, FCDoControl->exVerbose];

		If[	!OptionValue[SUND],
			heads = SelectFree[heads,SUND]
		];

		If[	!OptionValue[SUNF],
			heads = SelectFree[heads,SUNF]
		];

		list = Cases2[ex,heads];

		FCPrint[3,"Explicit: Relevant symbols in the expression: ", list, FCDoControl->exVerbose];

		repRules = Flatten[Map[#[opts] &, RulesForSymbolsWithExplicitOption]];

		listEval = list /. repRules;

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
