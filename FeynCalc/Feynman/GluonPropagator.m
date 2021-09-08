(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonPropagator													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Gluon propagator												*)

(* ------------------------------------------------------------------------ *)

GP::usage =
"GP is equivalent to GluonPropagator.";

GluonPropagator::usage =
"GluonPropagator[p, {\[Mu], a}, {\[Nu], b}] or GluonPropagator[p, \[Mu], a,
\[Nu], b] yields the gluon propagator.

GluonPropagator[p, {\[Mu]}, {\[Nu]}] or GluonPropagator[p, \[Mu], \[Nu]] omits
the SUNDelta.

GP can be used as an abbreviation of GluonPropagator.

The gauge and the dimension are determined by the options Gauge and Dimension.
The following settings of Gauge are possible:

 1 for the Feynman gauge;  
alpha for the general covariant gauge;
 Momentum[n] ,1} for the axial gauge.";

Begin["`Package`"]
End[]

Begin["`GluonPropagator`Private`"]

Options[GluonPropagator] = {
	CounterTerm -> False,
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False,
	Gauge -> 1,
	OPE -> False
};

GP = GluonPropagator;
Abbreviation[GluonPropagator] = HoldForm[GP];

GluonPropagator[a_, b_,c_, d_,e_, opt:OptionsPattern[]] :=
	GluonPropagator[a, {b,c}, {d,e}, opt]/;FreeQ[{a,b,c,d,e},Rule];

GluonPropagator[q_, {li_},{mu_},opt:OptionsPattern[]] :=
	GluonPropagator[-q, {li}, {mu}, opt] /;
	NumericalFactor[q] === -1;

GluonPropagator[pi_, mu_, nu_, OptionsPattern[]] :=
	GluonPropagator[pi, {mu}, {nu}]/;
	!MemberQ[{Rule, List}, Head[mu]] &&
	!MemberQ[{Rule, List}, Head[nu]];

GluonPropagator[pi_, {mui_,  ai___}, {nui_, bi___}, opt:OptionsPattern[]] :=
	Block[ {gauge, gluemass, dim, p, mu, nu, a, b, glp,n,ope, opepart, mud, nud, ad,
		bd, sundelta, p2, cou, gst, gmunu, pmu, pnu},
		gauge  = OptionValue[Gauge];
		dim    = OptionValue[Dimension];
		ope    = OptionValue[OPE];
		cou    = OptionValue[CounterTerm];
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
		If[ cou =!= False,
			p2 = Pair[p,p];
			gmunu = Pair[mu, nu];
			pmu = Pair[p, mu];
			pnu = Pair[p, nu];
			Which[
				cou === 1,
				(* put - sign by  hand ; wg. BPHZ *)
					glp = -I Sn gst^2 CA/Epsilon ( -19/6 p2 gmunu + 11/3 pmu pnu) sundelta,
				cou === 2,
					glp = -I Sn gst^2 CA/Epsilon ( -1/6 p2 gmunu - 1/3 pmu pnu) sundelta,
				cou === 3,
					glp = -I Sn gst^2 2 Tf/Epsilon (  4/3 p2 gmunu - 4/3 pmu pnu) sundelta,
				cou === 4,
					glp = -I Sn  gst^2 CA/Epsilon (-20/6 p2 gmunu + 10/3 pmu pnu) sundelta,
				cou === 5,
					glp = I Sn gst^2 CA/Epsilon (-20/6 p2 gmunu + 10/3 pmu pnu) sundelta +
					I Sn gst^2 Tf/Epsilon (  4/3 p2 gmunu -    4/3 pmu pnu) sundelta,
				cou === All,
					glp = CounterT (I Sn gst^2 CA/Epsilon (-20/6 p2 gmunu +    10/3 pmu pnu) sundelta +
					I Sn gst^2 Tf/Epsilon (  4/3 p2 gmunu -    4/3 pmu pnu) sundelta) +
					GluonPropagator[pi, {mui,  ai}, {nui, bi}, CounterTerm -> False, opt]
			],
			If[ ope =!= False,
				mud = FCGV[ToString[Unique["mli"]]];
				nud = FCGV[ToString[Unique["nli"]]];
				ad = FCGV[ToString[Unique["asi"]]];
				bd = FCGV[ToString[Unique["bsi"]]];
				glp = GluonPropagator[pi, {mui, ai}, {mud, ad}, OPE -> False, opt]*
				OPE Twist2GluonOperator[pi, {mud, ad}, {nud, bd}]*
				GluonPropagator[pi, {nud, bd}, {nui, bi}, OPE -> False, opt] +
				GluonPropagator[pi, {mui, ai}, {nui, bi}, OPE -> False, opt];
				glp = SUNDeltaContract[glp] /. Pair->PairContract /. PairContract->Pair,
				If[ Head[gauge] === List,
					n = gauge[[1]];
					If[ FreeQ[n, Momentum],
						n = Momentum[n, dim]
					];
					glp = I FeynAmpDenominator[PD[p, gluemass]] *
					sundelta (- Pair[mu, nu] + (Pair[n, mu] Pair[p,nu] + Pair[p, mu] Pair[n,nu]) / Pair[n, p] -
					(Pair[n, n] Pair[p,mu] Pair[p,nu]- gauge[[2]] Pair[p,p] Pair[n,mu] *
					Pair[n,nu]) /Pair[n,p]^2),
					glp  = I FeynAmpDenominator[PD[p, gluemass]] *
					sundelta (- Pair[mu, nu] + (1-gauge) Pair[p, mu] Pair[p, nu] *
					FeynAmpDenominator[PD[MomentumExpand[p], gluemass]]);
				];
			]
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
