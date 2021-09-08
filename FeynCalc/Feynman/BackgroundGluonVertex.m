(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: BackgroundGluonVertex											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Gluon vertex in the background field gauge						*)

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

Begin["`Package`"]
End[]

Begin["`BackgroundGluonVertex`Private`"]

Options[BackgroundGluonVertex] = {
	Dimension -> D,
	CouplingConstant -> SMP["g_s"],
	Gauge -> 1
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

FCPrint[1,"BackgroundGluonVertex.m loaded"];
End[]
