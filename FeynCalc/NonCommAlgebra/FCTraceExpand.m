(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCTraceExpand														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Expands traces using linearity							    *)

(* ------------------------------------------------------------------------ *)

FCTraceExpand::usage =
"FCTraceExpand[exp] expands traces of Dirac and $SU(N)$ matrices using
linearity of the trace. The traces themselves are not evaluated.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCTraceExpand`Private`"]

dotSimp::usage="";
propPres::usage="";
fctreVerbose::usage="";

Options[FCTraceExpand] = {
	DiracGammaExpand				-> True,
	PauliSigmaExpand				-> True,
	DiracTrace						-> True,
	PauliTrace						-> True,
	DotSimplify						-> True,
	FCE								-> False,
	FCI								-> False,
	FCTraceFactor					-> True,
	FCVerbose						-> False,
	Momentum 						-> All,
	PreservePropagatorStructures	-> False,
	SUNTrace 						-> True
};

FCTraceExpand[expr_, OptionsPattern[]] :=
	Block[{	ex, moms,res, diracTraces,diracTraces2, sunTraces,sunTraces2,
			pauliTraces, pauliTraces2},

		moms 		= OptionValue[Momentum];
		dotSimp 	= OptionValue[DotSimplify];
		propPres	= OptionValue[PreservePropagatorStructures];

		If [OptionValue[FCVerbose]===False,
			fctreVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fctreVerbose=OptionValue[FCVerbose]
			];
		];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];
		(*TODO: Add fast mode*)
		FCPrint[1, "FCTraceExpand: Entering.", FCDoControl->fctreVerbose];
		FCPrint[3, "FCTraceExpand: Entering with", ex, FCDoControl->fctreVerbose];

		If[ FreeQ2[ex,{DiracTrace, PauliTrace, SUNTrace}],
			Return[ex]
		];

		If[	OptionValue[SUNTrace],
			FCPrint[1, "FCTraceExpand: Expanding SU(N) traces.", FCDoControl->fctreVerbose];
			sunTraces = Cases2[ex, SUNTrace];
			sunTraces2 = sunTraces;

			If[	OptionValue[FCTraceFactor],
				sunTraces2 = FCTraceFactor/@sunTraces2
			];

			If[ sunTraces =!= {},
				ex = ex /. Dispatch[Thread[sunTraces -> traceexpand[sunTraces2]]]
			];
			FCPrint[3, "FCTraceExpand: Done expanding SU(N) traces: ", ex, FCDoControl->fctreVerbose];
		];

		If[	OptionValue[DiracTrace],
			FCPrint[1, "FCTraceExpand: Expanding Dirac traces.", FCDoControl->fctreVerbose];
			If [moms===All,
				diracTraces = Cases2[ex, DiracTrace],
				diracTraces = Select[Cases2[ex, DiracTrace], !FreeQ2[#, moms]&]
			];

			diracTraces2 = diracTraces;
			FCPrint[1, "FCTraceExpand: Done expanding Dirac traces: ", diracTraces2, FCDoControl->fctreVerbose];

			If [OptionValue[DiracGammaExpand],
				FCPrint[1, "FCTraceExpand: Applying DiracGammaExpand to the Dirac traces", FCDoControl->fctreVerbose];
				diracTraces2 = DiracGammaExpand[#,FCI->True]&/@diracTraces2;
				FCPrint[3, "FCTraceExpand: After applying DiracGammaExpand ", diracTraces2,  FCDoControl->fctreVerbose]
			];

			If[	OptionValue[FCTraceFactor],
				FCPrint[1, "FCTraceExpand: Applying FCTraceFactor to the Dirac traces", FCDoControl->fctreVerbose];
				diracTraces2 = FCTraceFactor/@diracTraces2;
				FCPrint[3, "FCTraceExpand: After applying FCTraceFactor ", diracTraces2,  FCDoControl->fctreVerbose]
			];

			If[ diracTraces =!= {},
				FCPrint[1, "FCTraceExpand: Expanding Dirac traces.", FCDoControl->fctreVerbose];
				ex = ex /. Dispatch[Thread[diracTraces -> traceexpand[diracTraces2]]];
				FCPrint[3, "FCTraceExpand: After the expansion of Dirac traces: ", ex,  FCDoControl->fctreVerbose]
			];
		];


		If[	OptionValue[PauliTrace],
			FCPrint[1, "FCTraceExpand: Expanding Pauli traces.", FCDoControl->fctreVerbose];
			If [moms===All,
				pauliTraces = Cases2[ex, PauliTrace],
				pauliTraces = Select[Cases2[ex, PauliTrace], !FreeQ2[#, moms]&]
			];

			pauliTraces2 = pauliTraces;
			FCPrint[1, "FCTraceExpand: Done expanding Pauli traces: ", pauliTraces2, FCDoControl->fctreVerbose];

			If [OptionValue[PauliSigmaExpand],
				FCPrint[1, "FCTraceExpand: Applying PauliSigmaExpand to the Pauli traces", FCDoControl->fctreVerbose];
				pauliTraces2 = PauliSigmaExpand[#,FCI->True]&/@pauliTraces2;
				FCPrint[3, "FCTraceExpand: After applying PauliSigmaExpand ", pauliTraces2,  FCDoControl->fctreVerbose]
			];

			If[	OptionValue[FCTraceFactor],
				FCPrint[1, "FCTraceExpand: Applying FCTraceFactor to the Pauli traces", FCDoControl->fctreVerbose];
				pauliTraces2 = FCTraceFactor/@pauliTraces2;
				FCPrint[3, "FCTraceExpand: After applying FCTraceFactor ", pauliTraces2,  FCDoControl->fctreVerbose]
			];

			If[ pauliTraces =!= {},
				FCPrint[1, "FCTraceExpand: Expanding Pauli traces.", FCDoControl->fctreVerbose];
				ex = ex /. Dispatch[Thread[pauliTraces -> traceexpand[pauliTraces2]]];
				FCPrint[3, "FCTraceExpand: After the expansion of Pauli traces: ", ex,  FCDoControl->fctreVerbose]
			];
		];

		FCPrint[3, "FCTraceExpand: Intermediate result ", ex,  FCDoControl->fctreVerbose];

		If[	OptionValue[FCTraceFactor],
			FCPrint[1, "FCTraceExpand: Applying FCTraceFactor ", FCDoControl->fctreVerbose];
			ex = FCTraceFactor[ex];
			FCPrint[3, "FCTraceExpand: After applying FCTraceFactor ", ex,  FCDoControl->fctreVerbose]
		];

		res = ex;

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCTraceExpand: Leaving.", FCDoControl->fctreVerbose];

		FCPrint[3, "FCTraceExpand: Leaving with ", res,  FCDoControl->fctreVerbose];

		res
	];

traceexpand[x_] :=
	x /. {DiracTrace->expandDirac, PauliTrace->expandPauli, SUNTrace->expandColor};

expandDirac[x_] :=
	If [dotSimp,
		Distribute[DiracTrace@(Expand[DotSimplify[x,PreservePropagatorStructures->propPres,FCI->True]])],
		Distribute[DiracTrace@(Expand[x])]
	];

expandPauli[x_] :=
	If [dotSimp,
		Distribute[PauliTrace@(Expand[DotSimplify[x,PreservePropagatorStructures->propPres,FCI->True]])],
		Distribute[PauliTrace@(Expand[x])]
	];

expandColor[x_] :=
	If [dotSimp,
		Distribute[SUNTrace@(Expand[DotSimplify[x,PreservePropagatorStructures->propPres,FCI->True]])],
		Distribute[SUNTrace@(Expand[x])]
	];

FCPrint[1,"FCTraceExpand.m loaded."];
End[]
