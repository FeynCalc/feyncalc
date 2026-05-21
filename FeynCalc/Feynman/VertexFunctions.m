(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: VertexFunctions													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Vertices														*)

(* ------------------------------------------------------------------------ *)

BackgroundGluonVertex::usage =
"BackgroundGluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}] yields the 3-gluon
vertex in the background field gauge, where the first set of arguments
corresponds to the external background field.   BackgroundGluonVertex[{p, mu,
a}, {q, nu, b}, {k, la, c}, {s, si, d}] yields the 4-gluon vertex, with {p, mu
,a} and {k, la, c} denoting the external background fields.

The gauge, dimension and the name of the coupling constant are determined by
the options Gauge, Dimension and CouplingConstant.

The Feynman rules are taken from L. Abbot NPB 185 (1981), 189-203; except that
all momenta are incoming. Note that Abbot's coupling constant convention is
consistent with the default setting of GluonVertex.";

GGV::usage =
"GGV is equivalent to GluonGhostVertex.";

GluonGhostVertex::usage =
"GluonGhostVertex[{p, mu, a}, {q, nu, b}, {k, rho, c}] or GluonGhostVertex[ p,
mu, a , q, nu, b , k, rho, c] yields the Gluon-Ghost vertex. The first
argument represents the gluon and the third argument the outgoing ghost field
(but incoming 4-momentum).

GGV can be used as an abbreviation of GluonGhostVertex.The dimension and the
name of the coupling constant are determined by the options Dimension and
CouplingConstant.";

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

QGV::usage =
"QGV is equivalent to QuarkGluonVertex.";

QuarkGluonVertex::usage =
"QuarkGluonVertex[mu, a] gives the Feynman rule for the quark-gluon vertex.

QGV can be used as an abbreviation of QuarkGluonVertex.

The dimension and the name of the coupling constant are determined by the
options Dimension and CouplingConstant.";

ScalarGluonVertex::usage =
"ScalarGluonVertex[{p}, {q}, {mu, a}] or ScalarGluonVertex[p,  q,  mu, a]
yields the scalar-scalar-gluon vertex, where p and q are incoming momenta.

ScalarGluonVertex[{mu, a}, {nu, b}] yields the scalar-scalar-gluon-gluon
vertex, where p and q are incoming momenta.

The dimension and the name of the coupling constant are determined by the
options Dimension and CouplingConstant.";

Begin["`Package`"]
End[]

Begin["`VertexFunctions`Private`"]

Options[BackgroundGluonVertex] = {
	Dimension -> D,
	CouplingConstant -> SMP["g_s"],
	Gauge -> 1
};

Options[GluonGhostVertex] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False
};


(* 3 - vertex *)
BackgroundGluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, x9_, opts:OptionsPattern[]] :=
	BackgroundGluonVertex[{x1,x2,x3}, {x4,x5,x6}, {x7,x8,x9} , opts] /;
	FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8,x9}]], Integer];

BackgroundGluonVertex[{pi_, mui_, ai_}, {qi_, nui_, bi_}, {ki_, lai_, ci_}, OptionsPattern[]] :=
	Block[ {alpha, dim, p, q, k, mu, nu, la, a, b, c, gl3v},
		alpha = OptionValue[Gauge];
		dim   = OptionValue[Dimension];
		{p,q,k}    = Map[Momentum[#, dim]&, {pi,qi,ki}];
		{mu,nu,la} = Map[LorentzIndex[#, dim]&, {mui,nui,lai}];
		{a,b,c}    = Map[SUNIndex[#]&, {ai,bi,ci}];
		gl3v = OptionValue[CouplingConstant] SUNF[a,b,c] MomentumCombine[(Pair[q - k, mu] Pair[nu, la] +
		Pair[k - p + 1/alpha q, nu] Pair[la, mu] + Pair[p - q -1/alpha k, la] Pair[mu, nu]),
		LeafCount -> 1000];
		gl3v
	];

(* 4 - vertex *)
BackgroundGluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, opts:OptionsPattern[]] :=
	BackgroundGluonVertex[{x1,x2}, {x3,x4}, {x5,x6}, {x7,x8}, opts] /;
	FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8}]], Integer];

BackgroundGluonVertex[_,x1_,x2_,_, x3_,x4_,_, x5_,x6_,_, x7_,x8_, opts:OptionsPattern[]] :=
	BackgroundGluonVertex[{x1,x2}, {x3,x4}, {x5,x6}, {x7,x8}, opts] /;
	FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8}]], Integer];

BackgroundGluonVertex[{___, mui_, ai_}, {___, nui_, bi_}, {___, lai_, ci_}, {___, rhoi_, di_},
	OptionsPattern[]] :=
	Block[ {alpha, dim, mu, nu, la, rho, a, b, c, d, e, gl4v},
		alpha = OptionValue[Gauge];
		dim   = OptionValue[Dimension];
		{mu,nu,la,rho} = Map[LorentzIndex[#, dim]&, {mui,nui,lai,rhoi}];
		{a,b,c,d}    = Map[SUNIndex[#]&, {ai,bi,ci,di}];
		e = SUNIndex[FCGV[ToString[Unique["u"]]]];
		gl4v = - I OptionValue[CouplingConstant]^2 (SUNF[a,b,e] SUNF[c,d,e] *
		(Pair[mu,la] Pair[nu,rho] - Pair[mu,rho] Pair[nu,la] + 1/alpha Pair[mu,nu] Pair[la,rho]) +
		SUNF[a,c,e] SUNF[b,d,e] (Pair[mu,nu] Pair[la,rho] - Pair[mu,rho] Pair[nu,la]) +
		SUNF[a,d,e] SUNF[b,c,e] (Pair[mu,nu] Pair[la,rho] - Pair[mu,la] Pair[nu,rho] -
		1/alpha Pair[mu,rho] Pair[nu,la]));
		gl4v
	];


GluonGhostVertex[{_, ai_}, {bi_}, {ki_, ci_}, opt:OptionsPattern[]] :=
	GluonGhostVertex[{FCGV["x"], FCGV["y"], ai}, {FCGV["z"],FCGV["h"],bi},
	{ki,FCGV["l"],ci}, opt] /; OptionValue[Explicit];

GGV = GluonGhostVertex;

GluonGhostVertex[a_,b_,c_, d_,e_,f_, g_,h_,i_, opt:OptionsPattern[]] :=
	GluonGhostVertex[{a,b,c},{d,e,f},{g,h,i},opt] /;
	FreeQ[Map[Head,{a,b,c,d,e,f,g,h,i}], Integer|Rule|RuleDelayed|List, Heads->False];

GluonGhostVertex[{_, mui_, ai_}, {___, bi_}, {ki_, ___, ci_}, opt:OptionsPattern[]] :=
	SUNF[SUNIndex[ai], SUNIndex[bi], SUNIndex[ci]] GluonGhostVertex[ki,mui,opt];

GluonGhostVertex[ki_, mui_, OptionsPattern[]] :=
	Block[ {dim, k, mu, re},
		dim   = OptionValue[Dimension];
		k = Momentum[ki,dim];
		mu = LorentzIndex[mui, dim];
		re = - OptionValue[CouplingConstant] Pair[k, mu];
		(* that is a matter of taste; the sign can be swapped between
			GhostPropagator and GluonGhostVertex.
			For the moment let's be consistent with Abbott (Nucl. Phys. B185 (1981)).
		*)
		(* re = -re;*)
		re = QCDFeynmanRuleConvention[GluonGhostVertex] re;
		re
	] /; OptionValue[Explicit];

GluonGhostVertex /:
	MakeBoxes[GluonGhostVertex[p3_,mu3_], TraditionalForm] :=
		RowBox[{SuperscriptBox[OverscriptBox["\[CapitalLambda]","~"],
		TBox[mu3]], "(", TBox[p3], ")"}];

GluonGhostVertex /:
	MakeBoxes[GluonGhostVertex[{_,_},{_,_},{p3_,mu3_}], TraditionalForm] :=
		RowBox[{SuperscriptBox[OverscriptBox["\[CapitalLambda]","~"],
		TBox[mu3]], "(", TBox[p3], ")"}];

Options[GluonVertex] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False
};

GV = GluonVertex;

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
	Block[ {gauge, dim, p, q, k, mu, nu, la, a, b, c, gl3v, expl},
		dim   = OptionValue[Dimension];
		expl  = OptionValue[Explicit];
		{a,b,c} = Map[SUNIndex[#]&, {ai,bi,ci}];
		{mu,nu,la} = Map[LorentzIndex[#, dim]&, {mui,nui,lai} /. ExplicitLorentzIndex[0]->0] // lorfix;
		{p,q,k}    = Map[Momentum[#, dim]&, {pi,qi,ki}]//momfix;
		gl3v = SUNF[a,b,c] Apply[GluonVertex, Join[{ {p,mu}, {q,nu}, {k,la} }, {opt}]];
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

GluonVertex[{(*p*)___, mui_, ai_}, {(*q*)___, nui_, bi_}, {(*r*)___, lai_, ci_}, {(*s*)___, sii_, di_}, OptionsPattern[]] :=
	Block[ {gauge, dim, mu, nu, la, si, a, b, c, d, e, gl4v, coup},
		coup  = OptionValue[CouplingConstant];
		dim   = OptionValue[Dimension];

		{mu,nu,la,si} = Map[LorentzIndex[#, dim]&, {mui,nui,lai,sii}/. ExplicitLorentzIndex[0]->0] // lorfix;
		{a,b,c,d}    = Map[SUNIndex[#]&, {ai,bi,ci,di}]//momfix;
		e = SUNIndex[FCGV[ToString[Unique["u"]]]];
		gl4v = - I coup^2 ( SUNF[a,b,e] SUNF[c,d,e] (Pair[mu,la] Pair[nu,si] - Pair[mu,si] Pair[nu,la]) +
		SUNF[a,c,e] SUNF[b,d,e] (Pair[mu,nu] Pair[la,si] - Pair[mu,si] Pair[nu,la]) +
		SUNF[a,d,e] SUNF[b,c,e] (Pair[mu,nu] Pair[la,si] - Pair[mu,la] Pair[nu,si]));

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

DeclareNonCommutative[QuarkGluonVertex];

Options[QuarkGluonVertex] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False,
	Polarization -> 0
};

QGV = QuarkGluonVertex;

QuarkGluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, x9_, y:OptionsPattern[]] :=
	QuarkGluonVertex[{x1,x2,x3}, {x4,x5,x6}, {x7,x8,x9} , y] /;
	FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8,x9}]], Integer|Rule];

QuarkGluonVertex[mui_, ai_/;Head[ai]=!=Rule, opt:OptionsPattern[]] :=
	QuarkGluonVertex[{Null, mui, ai}, {Null, Null, Null},
	{Null, Null, Null}, opt] /;    FreeQ[Union[Map[Head, {mui,ai}]], Integer];

QuarkGluonVertex[{p_, mui_, ai_}, {q_,___}, {k_,___}, OptionsPattern[]] :=
	Block[ {gauge, dim, mu, a, gl3v, coun, coup, ope, pol},
		coup  = OptionValue[CouplingConstant];
		dim   = OptionValue[Dimension];

		pol   = OptionValue[Polarization];
		mu = LorentzIndex[mui, dim];
		a  = SUNIndex[ai];
		gl3v = I coup DOT[SUNT[a], DiracGamma[mu, dim]];
		gl3v
	] /; OptionValue[Explicit] &&
					FreeQ[Union[Map[Head, {mui,ai}]], Integer];

QuarkGluonVertex[mui_, OptionsPattern[]] :=
	Block[ {gauge, dim, mu, a, gl3v, coun, coup, ope, pol},
		coup  = OptionValue[CouplingConstant];

		dim   = OptionValue[Dimension];

		pol   = OptionValue[Polarization];
		mu = LorentzIndex[mui, dim];

		gl3v =  I coup DiracGamma[mu, dim];

		gl3v
	] /; OptionValue[Explicit] && FreeQ[Union[Map[Head, {mui}]], Integer];

QuarkGluonVertex /:
	MakeBoxes[QuarkGluonVertex[mu1_], TraditionalForm] :=
		SuperscriptBox["Q", TBox[mu1] ];

QuarkGluonVertex /:
	MakeBoxes[QuarkGluonVertex[{_,mu1_, a_},{__},{__}, OptionsPattern[]], TraditionalForm] :=
		SubsuperscriptBox["Q",TBox[a], TBox[mu1] ];

Options[ScalarGluonVertex] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Gauge -> 1
};

(* 3 - vertex *)
ScalarGluonVertex[x1_,x2_,x3_,x4_, opts:OptionsPattern[]] :=
	ScalarGluonVertex[{x1}, {x2}, {x3,x4}, opts] /;
	FreeQ2[Union[Map[Head, {x1,x2,x3,x4}]], {Integer,List}] && Head[x4] =!= List;

ScalarGluonVertex[{pi_}, {qi_}, {mui_, ai_}, OptionsPattern[]] :=
	Block[ {alpha, dim, p, q, mu, a, b, c, gl3v},
		alpha = OptionValue[Gauge];
		dim   = OptionValue[Dimension];
		{p,q} = Map[Momentum[#, dim]&, {pi,qi}];
		{mu} = Map[LorentzIndex[#, dim]&, {mui}];
		{a} = Map[SUNIndex[#]&, {ai}];
		gl3v = I OptionValue[CouplingConstant] SUNT[a] MomentumCombine[Pair[p - q, mu],LeafCount -> 1000];
		gl3v
	];

(* 4 - vertex *)
ScalarGluonVertex[{mui_, ai_}, {nui_, bi_}, OptionsPattern[]] :=
	Block[ {alpha, dim, mu, nu, a, b, gl4v},
		dim   = OptionValue[Dimension];
		{mu,nu} = Map[LorentzIndex[#, dim]&, {mui,nui}];
		{a,b}  = Map[SUNIndex[#]&, {ai,bi}];
		gl4v   = I OptionValue[CouplingConstant]^2 (DOT[SUNT[a] , SUNT[b] + SUNT[b] , SUNT[a]]) Pair[mu, nu];
		gl4v
	];

FCPrint[1,"VertexFunctions.m loaded"];
End[]
