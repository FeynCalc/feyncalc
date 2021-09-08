(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TR  *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation  (see also DiracTrace) *)

(* ------------------------------------------------------------------------ *)

TR::usage=
"TR[exp] calculates the Dirac trace of exp. Depending on the setting of the
option SUNTrace also a trace over $SU(N)$ objects is performed.

The Mathematica build-in function Tr is overloaded to call TR if any of
DiracGamma, GA, GAD, GS or GSD are in the expression.

Tr[list] finds the trace of the matrix or tensor list.

Tr[list, f] finds a generalized trace, combining terms with f instead of Plus.

Tr[list, f, n] goes down to level n in list.

Tr[expression] calculates the DiracTrace, i.e., TR[expression] if any of
DiracGamma, GA, GAD, GS or GSD is present in expression.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TR`Private`"]

trVerbose::usage="";

Options[TR] = {
	Contract			-> True,
	DiracTraceEvaluate	-> True,
	EpsContract			-> True,
	Expand				-> True,
	Explicit			-> True,
	FCE					-> False,
	FCVerbose			-> True,
	Factoring			-> Automatic,
	Mandelstam			-> {},
	PairCollect			-> False,
	SUNNToCACF			-> False,
	SUNTrace			-> False,
	Schouten			-> 0,
	TraceOfOne			-> 4,
	West				-> True
};


TR[expr_, rul:OptionsPattern[]] :=
	Block[{ex, doot, diractr, dit, fcex, diractrev, sunntocacf,
		diracTraceOpts,sunTraceOpts,trOpts},

		If [OptionValue[FCVerbose]===False,
			trVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				trVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"TR: Entering with: ", expr, FCDoControl->trVerbose];

		ex=FeynCalcInternal[expr];

		diractrev		= OptionValue[DiracTraceEvaluate];
		sunntocacf		= OptionValue[SUNNToCACF];
		trOpts 			= Flatten[Join[{rul}, FilterRules[Options[TR], Except[{rul}]]]];
		diracTraceOpts	= Flatten[FilterRules[Options[trOpts], Options[DiracTrace]]];
		sunTraceOpts	= Flatten[FilterRules[Options[trOpts], Options[SUNTrace]]];

		If[!FreeQ2[ex, {CF,CA}],
			sunntocacf = True
		];

		If[OptionValue[Explicit],
			ex = Explicit[ex]
		];

		If[OptionValue[SUNTrace] && !FreeQ2[ex, {SUNIndex,ExplicitSUNIndex}],
			FCPrint[1,"TR: Computing the SU(N) trace.", FCDoControl->trVerbose];
			ex = DiracTrace[ex,diracTraceOpts];
			ex = SUNSimplify[ex, SUNNToCACF -> sunntocacf, SUNTrace -> True, Explicit -> False];
			ex = ex /. (DiracTraceEvaluate -> False) :>	(DiracTraceEvaluate -> diractrev) //
			SUNSimplify[#, SUNTrace -> False, SUNNToCACF -> sunntocacf, Explicit -> False]&,

			If[FreeQ[ex, SUNIndex|ExplicitSUNIndex],
				ex = DiracTrace[ex, diracTraceOpts];
				If[	OptionValue[SUNTrace],
					ex = SUNSimplify[ex, SUNTrace -> True, SUNNToCACF -> sunntocacf, Explicit -> False]
				];
						ex = ex /. (DiracTraceEvaluate -> False) :> (DiracTraceEvaluate -> diractrev) //
						SUNSimplify[#, SUNTrace -> False, SUNNToCACF -> sunntocacf, Explicit -> False]&,
						(*!FreeQ[ex, SUNIndex|ExplicitSUNIndex] -> !SUNTrace*)
						ex = DiracTrace[Trick[ex]// SUNSimplify[#, SUNNToCACF -> sunntocacf,
						SUNTrace -> OptionValue[SUNTrace],	Explicit -> OptionValue[Explicit]]&, diracTraceOpts]
			]
		];

		FCPrint[1,"TR: Computing the Dirac trace.", FCDoControl->trVerbose];
		FCPrint[4,"TR: Options for Dirac trace: ", {diracTraceOpts}, FCDoControl->trVerbose];
		diractr[y__] :=
			(DiracTrace @@ Join[{y}, diracTraceOpts]);
		ex = ex /. DiracTrace -> diractr;

		If[	OptionValue[FCE],
			ex = FCE[ex]
		];

		FCPrint[1,"TR: Leaving with: ", ex, FCDoControl->trVerbose];
		ex
	];

FCPrint[1,"TR.m loaded"];
End[]
