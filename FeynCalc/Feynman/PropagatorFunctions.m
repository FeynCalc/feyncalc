(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PropagatorFunctions												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Propagators													*)

(* ------------------------------------------------------------------------ *)

GHP::usage =
"GHP[p, a, b] gives the ghost propagator where a and b are the color indices.

GHP[p] omits the $\\delta _{ab}$.";

GhostPropagator::usage =
"GhostPropagator[p, a, b] gives the ghost propagator where a and b are the
color indices.

GhostPropagator[p] omits the $\\delta _{ab}$.

GHP can be used as an abbreviation of GhostPropagator.";

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

QP::usage =
"QP is an alias for QuarkPropagator.

QP[p] is the massless quark propagator.

QP[{p,m}] gives the  quark propagator with mass m.";

QuarkPropagator::usage =
"QuarkPropagator[p] is the massless quark propagator.

QuarkPropagator[{p, m}] gives the quark propagator with mass $m$.

QP can be used as an abbreviation of QuarkPropagator.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PropagatorFunctions`Private`"]

GHP = GhostPropagator;
GP	= GluonPropagator;
QP	= QuarkPropagator;

DeclareNonCommutative[QuarkPropagator];

Options[QuarkPropagator] = {
	Dimension -> D,
	Explicit -> False
};

Options[GhostPropagator] = {
	Dimension -> D,
	Explicit -> False
};

Options[GluonPropagator] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False,
	Gauge -> 1
};

GhostPropagator[pi_, OptionsPattern[]] :=
	Block[ {p, glp},
		p = Momentum[pi, OptionValue[Dimension]];
		glp  = I FeynAmpDenominator[PropagatorDenominator[p, 0]];
		QCDFeynmanRuleConvention[GhostPropagator] glp
	] /; OptionValue[Explicit];

GhostPropagator[pi_, ai_, bi_, OptionsPattern[]] :=
	Block[ {p, a, b, glp},
		p = Momentum[pi, OptionValue[Dimension]];
		a = SUNIndex[ai];
		b = SUNIndex[bi];
		glp  = I FeynAmpDenominator[PropagatorDenominator[p, 0]] SUNDelta[a, b];
		QCDFeynmanRuleConvention[GhostPropagator] glp
	] /; OptionValue[Explicit];

GhostPropagator /:
	MakeBoxes[GhostPropagator[p_,a_,b_], TraditionalForm] :=
		RowBox[{SubscriptBox["\[CapitalPi]", TBox[a,b]],"(", TBox[p], ")"}];

GhostPropagator /:
	MakeBoxes[GhostPropagator[p_], TraditionalForm] :=
		RowBox[{SubscriptBox["\[CapitalPi]", "u"], "(", TBox[p], ")" }];

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


QuarkPropagator[pi:Except[_?OptionQ], opt:OptionsPattern[]] :=
	QuarkPropagator[{pi,0}, opt]/; Head[pi]=!=List;

QuarkPropagator[{pi_, m_},  OptionsPattern[]] :=
	Block[ {dim, re, ope, pol, cou, cop, loo},
		dim    = OptionValue[Dimension];
		re = I (DiracGamma[Momentum[pi, dim], dim]+m) FeynAmpDenominator[PropagatorDenominator[MomentumExpand[Momentum[pi,dim]], m]];
		re
	]/; OptionValue[Explicit];


QuarkPropagator /:
	MakeBoxes[QuarkPropagator[{p_,_}, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubscriptBox["\[CapitalPi]","q"],"(", TBox[p], ")"}];

FCPrint[1,"PropagatorFunctions.m loaded"];
End[]
