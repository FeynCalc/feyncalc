(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScalarGluonVertex												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Scalar gluon vertex											*)

(* ------------------------------------------------------------------------ *)

ScalarGluonVertex::usage =
"ScalarGluonVertex[{p}, {q}, {\[Mu], a}] or ScalarGluonVertex[p,  q,  \[Mu], a]
yields the scalar-scalar-gluon vertex, where p and q are incoming momenta.

ScalarGluonVertex[{\[Mu], a}, {\[Nu], b}] yields the scalar-scalar-gluon-gluon
vertex, where p and q are incoming momenta.

The dimension and the name of the coupling constant are determined by the
options Dimension and CouplingConstant.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ScalarGluonVertex`Private`"]

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

FCPrint[1,"ScalarGluonVertex.m loaded"];
End[]
