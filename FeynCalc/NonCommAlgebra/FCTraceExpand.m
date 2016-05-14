(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCTraceExpand														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Expands traces using linearity							    *)

(* ------------------------------------------------------------------------ *)

FCTraceExpand::usage =
"FCTraceExpand[expr]  expands traces of Dirac and SU(N) matrices \
using linearity of the trace. The traces themselves are not \
evaluated.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCTraceExpand`Private`"]

dotSimp::usage="";
propPres::usage="";

Options[FCTraceExpand] = {
	DiracGammaExpand -> True,
	DiracTrace -> True,
	DotSimplify -> True,
	FCI -> False,
	FCTraceFactor -> True,
	Momentum -> All,
	PreservePropagatorStructures -> False,
	SUNTrace -> True
};

FCTraceExpand[expr_, OptionsPattern[]] :=
	Block[ {ex, moms,res, diracTraces,diracTraces2, sunTraces,sunTraces2},

		moms = OptionValue[Momentum];
		dotSimp = OptionValue[DotSimplify];
		propPres = OptionValue[PreservePropagatorStructures];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[ FreeQ2[ex,{DiracTrace, SUNTrace}],
			Return[ex]
		];

		If[	OptionValue[SUNTrace],
			sunTraces = Cases2[ex, SUNTrace];
			sunTraces2 = sunTraces;

			If[	OptionValue[FCTraceFactor],
				sunTraces2 = FCTraceFactor/@sunTraces2
			];

			If[ sunTraces =!= {},
				ex = ex /. Dispatch[Thread[sunTraces -> traceexpand[sunTraces2]]]
			];
		];

		If[	OptionValue[DiracTrace],
			If [moms===All,
				diracTraces = Cases2[ex, DiracTrace],
				diracTraces = Select[Cases2[ex, DiracTrace], !FreeQ2[#, moms]&]
			];

			diracTraces2 = diracTraces;

			If [OptionValue[DiracGammaExpand],
				diracTraces2 = DiracGammaExpand/@diracTraces2
			];

			If[	OptionValue[FCTraceFactor],
				diracTraces2 = FCTraceFactor/@diracTraces2
			];

			If[ diracTraces =!= {},
				ex = ex /. Dispatch[Thread[diracTraces -> traceexpand[diracTraces2]]]
			];
		];

		If[	OptionValue[FCTraceFactor],
			ex = FCTraceFactor[ex]
		];

		res = ex;

		res
	];

traceexpand[x_] :=
	x /. {DiracTrace->expandDirac, SUNTrace->expandColor};

expandDirac[x_] :=
	If [dotSimp,
		Distribute[DiracTrace@(Expand[DotSimplify[x,PreservePropagatorStructures->propPres]])],
		Distribute[DiracTrace@(Expand[x])]
	]

expandColor[x_] :=
	If [dotSimp,
		Distribute[SUNTrace@(Expand[DotSimplify[x,PreservePropagatorStructures->propPres]])],
		Distribute[SUNTrace@(Expand[x])]
	]

FCPrint[1,"FCTraceExpand.m loaded."];
End[]
