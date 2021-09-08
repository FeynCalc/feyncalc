(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonVertex														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Gluon vertices													*)

(* ------------------------------------------------------------------------ *)

GV::usage =
"GV is equivalent to GluonVertex.";

GluonVertex::usage =
"GluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}] or GluonVertex[p, mu, a, q,
nu, b, k, la, c] yields the 3-gluon vertex.    

GluonVertex[{p, mu}, {q, nu}, {k, la}] yields the 3-gluon vertex without color
structure and the coupling constant.

GluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}, {s, si, d}] or
GluonVertex[{mu, a}, {nu, b}, {la, c}, {si, d}] or GluonVertex[p, mu, a, q,
nu, b, k, la, c , s, si, d] or GluonVertex[mu, a, nu, b, la, c, si, d] yields
the 4-gluon vertex.

GV can be used as an abbreviation of GluonVertex.

The dimension and the name of the coupling constant are determined by the
options Dimension and CouplingConstant. All momenta are flowing into the
vertex.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`GluonVertex`Private`"]

Options[GluonVertex] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False,
	OPE -> False
};

GV = GluonVertex;
Abbreviation[GluonVertex] = HoldForm[GV];

lorfix[w_] :=
	MomentumCombine[w,LeafCount -> 1000] /. LorentzIndex -> lorf /. lorf -> LorentzIndex;
lorf[y_lorf,___] :=
	y;
lorf[y_Momentum,___] :=
	y;
momfix[v_] :=
	MomentumCombine[v,LeafCount -> 1000]/.Momentum->momf/.momf->Momentum;
momf[y_momf,___] :=
	y;

(* 3 - vertex *)
GluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, x9_, y___Rule] :=
	GluonVertex[{x1,x2,x3}, {x4,x5,x6}, {x7,x8,x9} , y] /;
	FreeQ2[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8,x9}]], {Integer,Rule,RuleDelayed}];

GluonVertex[{pi_, mui_, ai_}, {qi_, nui_, bi_}, {ki_, lai_, ci_}, opt:OptionsPattern[]] :=
	Block[ {gauge, dim, p, q, k, mu, nu, la, a, b, c, gl3v, ope, expl},
		dim   = OptionValue[Dimension];
		ope   = OptionValue[OPE];
		expl  = OptionValue[Explicit];
		{a,b,c} = Map[SUNIndex[#]&, {ai,bi,ci}];
		{mu,nu,la} = Map[LorentzIndex[#, dim]&, {mui,nui,lai} /. ExplicitLorentzIndex[0]->0] // lorfix;
		{p,q,k}    = Map[Momentum[#, dim]&, {pi,qi,ki}]//momfix;
		gl3v = SUNF[a,b,c] Apply[GluonVertex, Join[{ {p,mu}, {q,nu}, {k,la} },
		Select[{opt}, FreeQ[#, OPE]&]]];
		If[ ope,
			gl3v = gl3v + OPE Twist2GluonOperator[{pi, mui, ai}, {qi, nui, bi}, {ki, lai, ci}]
		];
		gl3v
	];

GluonVertex[{pi_, mui_}, {qi_, nui_}, {ki_, lai_}, OptionsPattern[]] :=
	Block[ {coup, dim, p, q, k, mu, nu, la},
		dim   = OptionValue[Dimension];
		coup  = OptionValue[CouplingConstant];
		{mu,nu,la} = Map[LorentzIndex[#, dim]&, {mui,nui,lai} /. ExplicitLorentzIndex[0]->0] // lorfix;
		{p,q,k}    = Map[Momentum[#, dim]&, {pi,qi,ki}]//momfix;
		coup MomentumCombine[(Pair[q - k, mu] Pair[nu, la] + Pair[k - p, nu] Pair[la, mu] +
		Pair[p - q, la] Pair[mu, nu]),LeafCount -> 1000]
	]/; OptionValue[Explicit];


(* 4 - vertex *)
GluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, opts:OptionsPattern[]] :=
	GluonVertex[{x1,x2}, {x3,x4}, {x5,x6}, {x7,x8}, opts] /;
	FreeQ2[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8}]], {Integer, Rule, RuleDelayed}];

GluonVertex[_,x1_,x2_,_, x3_,x4_,_, x5_,x6_,_, x7_,x8_,    opts:OptionsPattern[]] :=
	GluonVertex[{x1,x2}, {x3,x4}, {x5,x6}, {x7,x8}, opts] /;
	FreeQ2[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8}]], {Integer, Rule, RuleDelayed}];

GluonVertex[{p___, mui_, ai_}, {q___, nui_, bi_}, {r___, lai_, ci_}, {s___, sii_, di_}, OptionsPattern[]] :=
	Block[ {gauge, dim, mu, nu, la, si, a, b, c, d, e, gl4v, ope, coup},
		coup  = OptionValue[CouplingConstant];
		dim   = OptionValue[Dimension];
		ope   = OptionValue[OPE];
		{mu,nu,la,si} = Map[LorentzIndex[#, dim]&, {mui,nui,lai,sii}/. ExplicitLorentzIndex[0]->0] // lorfix;
		{a,b,c,d}    = Map[SUNIndex[#]&, {ai,bi,ci,di}]//momfix;
		e = SUNIndex[FCGV[ToString[Unique["u"]]]];
		gl4v = - I coup^2 ( SUNF[a,b,e] SUNF[c,d,e] (Pair[mu,la] Pair[nu,si] - Pair[mu,si] Pair[nu,la]) +
		SUNF[a,c,e] SUNF[b,d,e] (Pair[mu,nu] Pair[la,si] - Pair[mu,si] Pair[nu,la]) +
		SUNF[a,d,e] SUNF[b,c,e] (Pair[mu,nu] Pair[la,si] - Pair[mu,la] Pair[nu,si]));
		If[ ope,
			gl4v = gl4v + OPE Twist2GluonOperator[{p, mui, ai},    {q, nui, bi}, {r, lai, ci}, {s, sii, di}]
		];
		gl4v
	]/; OptionValue[Explicit];

GluonVertex /:
	MakeBoxes[GluonVertex[{p1_,mu1_},{p2_,mu2_},{p3_,mu3_}], TraditionalForm] :=
		RowBox[{SuperscriptBox["V",TBox[mu1,mu2,mu3]],"(", TBox[p1,", ",p2,", ", p3], ")"}];

GluonVertex /:
	MakeBoxes[GluonVertex[{_,mu1_},{_,mu2_},{_,mu3_},{_,mu4_}],    TraditionalForm] :=
		SuperscriptBox["V",TBox[mu1,mu2,mu3,mu4]];

GluonVertex /:
	MakeBoxes[GluonVertex[{p1_,mu1_, a_},{p2_,mu2_,b_},{p3_,mu3_,c_},{p4_,mu4_,d_}], TraditionalForm] :=
		RowBox[{SubsuperscriptBox["V",TBox[a,b,c,d],TBox[mu1,mu2,mu3,mu4] ],
		"(", TBox[p1,", ",p2,", ", p3,", ",p4], ")"}]

FCPrint[1,"GluonVertex.m loaded"];
End[]
