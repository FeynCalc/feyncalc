(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TR  *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation  (see also DiracTrace) *)

(* ------------------------------------------------------------------------ *)

TR::usage=
"TR[exp] calculates the Dirac trace of exp. Depending on the setting of \
the option SUNTrace also a trace over SU(N) objects is performed. \
TR is identical to DiracTrace, up to the default setting of DiracTraceEvaluate.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TR`Private`"]

trVerbose::usage="";

Options[ TR ] = {
	Contract -> 400000,
	DiracTraceEvaluate -> True,
	EpsContract           -> True,
	Explicit           -> True,
	Expand			   -> True,
	Factoring          -> Automatic,
	FCVerbose			-> True,
	FeynCalcExternal   -> False,
	Mandelstam         -> {},
	PairCollect			-> False,
	SUNNToCACF			-> False,
	SUNTrace           -> False,
	Schouten           -> 0,
	TraceOfOne         -> 4,
	West				-> True
};


TR[x_, rul:OptionsPattern[]] :=
	Block[{tt=FeynCalcInternal[x], doot, diractr, dit, fcex, diractrev, sunntocacf,
		diracTraceOpts,sunTraceOpts,trOpts},

		If [OptionValue[FCVerbose]===False,
			trVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				trVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!FreeQ2[{x}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		FCPrint[1,"TR: Entering with: ", tt, FCDoControl->trVerbose];

		diractrev = OptionValue[DiracTraceEvaluate];
		sunntocacf = OptionValue[SUNNToCACF];
		trOpts = Flatten[Join[{rul}, FilterRules[Options[TR], Except[{rul}]]]];
		diracTraceOpts = Flatten[FilterRules[Options[trOpts], Options[DiracTrace]]];
		sunTraceOpts = Flatten[FilterRules[Options[trOpts], Options[SUNTrace]]];

		If[!FreeQ[x,CF|CA],
			sunntocacf = True
		];

		If[OptionValue[Explicit],
			tt = Explicit[tt]
		];

		If[OptionValue[SUNTrace] && !FreeQ2[tt, {SUNIndex,ExplicitSUNIndex}],
			FCPrint[1,"TR: Computing the SU(N) trace.", FCDoControl->trVerbose];
			tt = DiracTrace[tt,diracTraceOpts];
			tt = SUNSimplify[tt, SUNNToCACF -> sunntocacf, SUNTrace -> True, Explicit -> False];
			tt = tt /. (DiracTraceEvaluate -> False) :>	(DiracTraceEvaluate -> diractrev) //
			SUNSimplify[#, SUNTrace -> False, SUNNToCACF -> sunntocacf, Explicit -> False]&,

			If[FreeQ[tt, SUNIndex|ExplicitSUNIndex],
				tt = DiracTrace[tt, diracTraceOpts];
				If[	OptionValue[SUNTrace],
					tt = SUNSimplify[tt, SUNTrace -> True, SUNNToCACF -> sunntocacf, Explicit -> False]
				];
						tt = tt /. (DiracTraceEvaluate -> False) :> (DiracTraceEvaluate -> diractrev) //
						SUNSimplify[#, SUNTrace -> False, SUNNToCACF -> sunntocacf, Explicit -> False]&,
						(*!FreeQ[tt, SUNIndex|ExplicitSUNIndex] -> !SUNTrace*)
						tt = DiracTrace[Trick[tt]// SUNSimplify[#, SUNNToCACF -> sunntocacf,
						SUNTrace -> OptionValue[SUNTrace],	Explicit -> OptionValue[Explicit]]&, diracTraceOpts]
			]
		];

		FCPrint[1,"TR: Computing the Dirac trace.", FCDoControl->trVerbose];
		FCPrint[4,"TR: Options for Dirac trace: ", {diracTraceOpts}, FCDoControl->trVerbose];
		diractr[y__] :=
			(DiracTrace @@ Join[{y}, diracTraceOpts]);
		tt = tt /. DiracTrace -> diractr;

		If[	OptionValue[FeynCalcExternal],
			tt = FeynCalcExternal[tt]
		];

		FCPrint[1,"TR: Leaving with: ",tt, FCDoControl->trVerbose];
		tt
	];

FCPrint[1,"TR.m loaded"];
End[]
