(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonPropagator													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Gluon propagator												*)

(* ------------------------------------------------------------------------ *)

GP::usage =
"GP is equivalent to GluonPropagator.";

GluonPropagator::usage =
"GluonPropagator[p, {mu, a}, {nu, b}] or GluonPropagator[p, mu, a, nu, b]
yields the gluon propagator.

GluonPropagator[p, {mu}, {nu}] or GluonPropagator[p, mu, nu] omits the
SUNDelta.

GP can be used as an abbreviation of GluonPropagator.

The gauge and the dimension are determined by the options Gauge and Dimension.
The following settings of Gauge are possible:

- 1 for the Feynman gauge
- alpha for the general covariant gauge
- {Momentum[n] ,1} for the axial gauge";

Begin["`Package`"]
End[]

Begin["`GluonPropagator`Private`"]

Options[GluonPropagator] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False,
	Gauge -> 1
};

GP = GluonPropagator;
Abbreviation[GluonPropagator] = HoldForm[GP];

GluonPropagator[a_, b_,c_, d_,e_, opt:OptionsPattern[]] :=
	GluonPropagator[a, {b,c}, {d,e}, opt]/;FreeQ[{a,b,c,d,e},Rule];

GluonPropagator[q_, {li_},{mu_},opt:OptionsPattern[]] :=
	GluonPropagator[-q, {li}, {mu}, opt] /;
	NumericalFactor[q] === -1;

GluonPropagator[pi_, mu_, nu_, opt:OptionsPattern[]] :=
	GluonPropagator[pi, {mu}, {nu}, opt]/;
	!MemberQ[{Rule, List}, Head[mu]] &&
	!MemberQ[{Rule, List}, Head[nu]];

GluonPropagator[pi_, {mui_,  ai___}, {nui_, bi___}, OptionsPattern[]] :=
	Block[ {gauge, gluemass, dim, p, mu, nu, a, b, glp,n,ope, opepart, mud, nud, ad,
		bd, sundelta, p2, cou, gst, gmunu, pmu, pnu},
		gauge  = OptionValue[Gauge];
		dim    = OptionValue[Dimension];
		gst    = OptionValue[CouplingConstant];

		mu = LorentzIndex[mui, dim];
		nu = LorentzIndex[nui, dim];
		If[ Head[pi]===List,
			p = Momentum[pi[[1]], dim];
			gluemass = pi[[2]],
			p = Momentum[pi, dim];
			gluemass = 0;
		];
		If[ Length[{ai}] === Length[{bi}] === 1,
			sundelta = SUNDelta[SUNIndex[ai],  SUNIndex[bi]],
			sundelta = 1
		];


		If[ Head[gauge] === List,
			n = gauge[[1]];
			If[ FreeQ[n, Momentum],
				n = Momentum[n, dim]
			];
			glp = I FeynAmpDenominator[PD[p, gluemass]] *
			sundelta (- Pair[mu, nu] + (Pair[n, mu] Pair[p,nu] + Pair[p, mu] Pair[n,nu])  FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[n, p], 0, {1, 1}]] -
			(Pair[n, n] Pair[p,mu] Pair[p,nu]- gauge[[2]] Pair[p,p] Pair[n,mu] *
			Pair[n,nu]) FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[n, p], 0, {2, 1}]]),
			glp  = I FeynAmpDenominator[PD[p, gluemass]] *
			sundelta (- Pair[mu, nu] + (1-gauge) Pair[p, mu] Pair[p, nu] *
			FeynAmpDenominator[PD[MomentumExpand[p], gluemass]]);
		];


		glp
	]/; OptionValue[Explicit];

GluonPropagator /:
	MakeBoxes[GluonPropagator[p_,{mu_,a_},{nu_,b_}, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubsuperscriptBox["\[CapitalPi]",TBox[a,b], TBox[mu,nu]], "(", TBox[p], ")"}];

GluonPropagator /:
	MakeBoxes[GluonPropagator[p_,{mu_},{nu_}, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubsuperscriptBox["\[CapitalPi]", "g", TBox[mu,nu]], "(", TBox[p], ")"}];

FCPrint[1,"GluonPropagator.m loaded"];
End[]
