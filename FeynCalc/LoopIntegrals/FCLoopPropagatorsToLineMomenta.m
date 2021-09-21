(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopPropagatorsToLineMomenta									*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Line momenta from FeynAmpDenominators						*)

(* ------------------------------------------------------------------------ *)

FCLoopPropagatorsToLineMomenta::usage =
"FCLoopPropagatorsToLineMomenta[{prop1, prop2, ...}] is an auxiliary function
that extracts line momenta flowing through the given list of propagators.";

AuxiliaryMomenta::usage =
"AuxiliaryMomenta is an option of FCLoopPropagatorsToLineMomenta,
FCLoopIntegralToGraph and other functions. It specifies auxiliary momenta that
do not correspond to external legs, i.e. don't really flow through the lines,
e.g. $n$ and $\\bar{n}$ in SCET or $v$ in HQET.";

FCLoopPropagatorsToLineMomenta::failmsg =
"FCLoopPropagatorsToLineMomenta has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCLoopPropagatorsToLineMomenta`Private`"]

ptlmVerbose::usage="";
auxMoms::usage="";

Options[FCLoopPropagatorsToLineMomenta] = {
	AuxiliaryMomenta	-> {},
	FCE					-> True,
	FCI					-> False,
	FromGFAD			-> True,
	FCVerbose			-> False,
	Expanding			-> True,
	MomentumCombine		-> True
};

FCLoopPropagatorsToLineMomenta[expr_, OptionsPattern[]] :=
	Block[{ex, res, fadsList, fadsListEval, repRule, fad},


		If[	OptionValue[FCVerbose] === False,
			ptlmVerbose = $VeryVerbose,
			If[	MatchQ[OptionValue[FCVerbose], _Integer],
				ptlmVerbose = OptionValue[FCVerbose]
			];
		];

		auxMoms = OptionValue[AuxiliaryMomenta];
		If[ Head[auxMoms]=!=List,
			Message[FCLoopPropagatorsToLineMomenta::failmsg, "The value of the option AuxiliaryMomenta must be a list."];
			Abort[]

		];
		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[1, "FCLoopPropagatorsToLineMomenta: Entering.", FCDoControl->ptlmVerbose];
		FCPrint[3, "FCLoopPropagatorsToLineMomenta: Entering with: ", ex,  FCDoControl->ptlmVerbose];

		If[	OptionValue[MomentumCombine],
			ex = MomentumCombine[ex,FCI->True]
		];

		FCPrint[3, "FCLoopPropagatorsToLineMomenta: After MomentumCombine: ", ex,  FCDoControl->ptlmVerbose];

		If[	OptionValue[FromGFAD],
			ex = FromGFAD[ex,FCI->True]
		];

		FCPrint[3, "FCLoopPropagatorsToLineMomenta: After FromGFAD: ", ex,  FCDoControl->ptlmVerbose];

		If[	!MatchQ[ex, {FeynAmpDenominator__}],
			Message[FCLoopPropagatorsToLineMomenta::failmsg, "The input expression is not a valid list of propagators"];
			Abort[]
		];

		ex = ex /. FeynAmpDenominator[(a_PropagatorDenominator)..] :> FeynAmpDenominator[a];

		res = ex /. FeynAmpDenominator-> lineMomentumFromPropagator;

		(* Handle stuff like SFAD[{{I p1 + I p3, 0}, {-mb^2, 1}, 1}] *)
		If[!FreeQ[res,Complex],
			res = Replace[Factor[res], {Complex[0, 1] a_, b_} :> {a, b}, 1]
		];

		FCPrint[3, "FCLoopPropagatorsToLineMomenta: After lineMomentumFromPropagator", res,  FCDoControl->ptlmVerbose];

		If[	!FreeQ2[res,{FeynAmpDenominator,lineMomentumFromPropagator}],
			Message[FCLoopPropagatorsToLineMomenta::failmsg, "Some propagators could not be converted to line momenta."];
			Abort[]
		];

		If[	OptionValue[Expanding],
			res = ExpandAll[res]
		];

		res = Join[Transpose[res],{ex}];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCLoopPropagatorsToLineMomenta: Leaving.", FCDoControl->ptlmVerbose];
		FCPrint[1, "FCLoopPropagatorsToLineMomenta: Leaving with:", res, FCDoControl->ptlmVerbose];

		res
	];

(*FAD quadratic, 1/[p^2-m^2]*)
lineMomentumFromPropagator[PropagatorDenominator[_. Momentum[mom_, ___], mass_]]:=
	{mom, -mass^2}/; FreeQ2[mom,auxMoms];

(*SFAD squared, 1/[p^2-m^2] or 1/[p^2+m^2] *)
lineMomentumFromPropagator[StandardPropagatorDenominator[_. Momentum[mom_, ___], 0, massSq_, {_, _}]]:=
	{mom, massSq}/; FreeQ2[mom,auxMoms];

(*CFAD squared, 1/[p^2-m^2] or 1/[p^2+m^2] *)
lineMomentumFromPropagator[CartesianPropagatorDenominator[_. CartesianMomentum[mom_, ___], 0, massSq_, {_, _}]]:=
	{mom, massSq}/; FreeQ2[mom,auxMoms];


(*-------------------------------------------------------------*)


(*SFAD eikonal 1/[+/- x p.q] = 2/x 1/[+/- 2 p.q]   *)
lineMomentumFromPropagator[StandardPropagatorDenominator[0,	pref_. Pair[Momentum[a_, ___],
	Momentum[b_, ___]], 0, {_, _}]]:=
	If[	Internal`SyntacticNegativeQ[pref],
		{a - b, 0},
		{a + b, 0}
	]/; FreeQ2[{a,b},auxMoms];

(*SFAD eikonal 1/[+/- x p.q +/- m^2]  *)
lineMomentumFromPropagator[StandardPropagatorDenominator[0,	pref_. Pair[Momentum[a_, ___],
	Momentum[b_, ___]], massSq_, {_, _}]]:=
	If[	Internal`SyntacticNegativeQ[pref],
		{a - pref/2 b, massSq},
		{a + pref/2 b, massSq}
	]/; massSq=!=0 && FreeQ2[{a,b},auxMoms];

(*SFAD eikonal 1/[p^2 +/- x p.q +/- m^2]  *)
lineMomentumFromPropagator[StandardPropagatorDenominator[Momentum[mom_, ___],	pref_. Pair[Momentum[mom_, ___],
	Momentum[b_, ___]], massSq_, {_, _}]]:=
	{mom + pref/2 b, massSq}/; FreeQ2[mom,auxMoms];


(*SFAD eikonal 1/[-p^2 +/- x p.q +/- m^2]  *)
lineMomentumFromPropagator[StandardPropagatorDenominator[Complex[0,1] Momentum[mom_, ___],	pref_. Pair[Momentum[mom_, ___],
	Momentum[b_, ___]], massSq_, {_, _}]]:=
	{mom - pref/2 b, massSq}/; FreeQ2[mom,auxMoms];

(*SFAD eikonal 1/[+/- v.q]  *)
lineMomentumFromPropagator[StandardPropagatorDenominator[0,	pref_. Pair[Momentum[a_, ___],
	Momentum[b_, ___]], massSq_, {_, _}]]:=
	If[	Internal`SyntacticNegativeQ[pref],
		{-a, massSq},
		{a, massSq}
	]/; MemberQ[auxMoms,b];


(*-------------------------------------------------------------*)

(*CFAD eikonal 1/[+/- x p.q] = 2/x 1/[+/- 2 p.q]  *)
lineMomentumFromPropagator[CartesianPropagatorDenominator[0, pref_. CartesianPair[CartesianMomentum[a_, ___],
	CartesianMomentum[b_, ___]], 0, {_, _}]]:=
	If[	Internal`SyntacticNegativeQ[pref],
		{a - b, 0},
		{a + b, 0}
	]/; FreeQ2[{a,b},auxMoms];

(*CFAD eikonal 1/[+/- x p.q +/- m^2]  *)
lineMomentumFromPropagator[CartesianPropagatorDenominator[0, pref_. CartesianPair[CartesianMomentum[a_, ___],
	CartesianMomentum[b_, ___]], massSq_, {_, _}]]:=
	If[	Internal`SyntacticNegativeQ[pref],
		{a - pref/2 b, massSq},
		{a + pref/2 b, massSq}
	]/; massSq=!=0 && FreeQ2[{a,b},auxMoms];

(*CFAD eikonal 1/[p^2 +/- x p.q +/- m^2]  *)
lineMomentumFromPropagator[CartesianPropagatorDenominator[CartesianMomentum[mom_, ___],	pref_. CartesianPair[CartesianMomentum[mom_, ___],
	CartesianMomentum[b_, ___]], massSq_, {_, _}]]:=
	{mom + pref/2 b, massSq}/; FreeQ2[mom,auxMoms];

(*CFAD eikonal 1/[p^2 +/- x p.q +/- m^2]  *)
lineMomentumFromPropagator[CartesianPropagatorDenominator[Complex[0,1] CartesianMomentum[mom_, ___],	pref_. CartesianPair[CartesianMomentum[mom_, ___],
	CartesianMomentum[b_, ___]], massSq_, {_, _}]]:=
	{I mom + pref/2 b, massSq}/; FreeQ2[mom,auxMoms];

(*CFAD eikonal 1/[+/- v.q]  *)
lineMomentumFromPropagator[CartesianPropagatorDenominator[0,	pref_. CartesianPair[CartesianMomentum[a_, ___],
	CartesianMomentum[b_, ___]], massSq_, {_, _}]]:=
	If[	Internal`SyntacticNegativeQ[pref],
		{-a, massSq},
		{a, massSq}
	]/; MemberQ[auxMoms,b];


FCPrint[1,"FCLoopPropagatorsToLineMomenta.m loaded."];
End[]
