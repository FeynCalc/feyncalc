(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScalarGluonVertex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ScalarGluonVertex *)

(* ------------------------------------------------------------------------ *)

ScalarGluonVertex::usage =
"ScalarGluonVertex[{p}, {q}, {mu,a}] or
ScalarGluonVertex[ p,  q,  mu, a ] yields the
scalar-scalar-gluon vertex (p and q are incoming momenta).\n\n

ScalarGluonVertex[{mu,a}, {nu,b}]
yields the scalar-scalar-gluon-gluon vertex
(p and q are incoming momenta).\n\n

The dimension  and the name of the coupling constant
are determined by the options Dimension and CouplingConstant.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ScalarGluonVertex`Private`"]

Options[ScalarGluonVertex] = {
	CouplingConstant -> Gstrong,
	Dimension -> D,
	Gauge -> 1
};

{l, c} = MakeFeynCalcPrivateContext /@ {"l", "c"};

ScalarGluonVertex[x___, i_Integer, y___] :=
	ScalarGluonVertex[x, l[i], c[i], y];

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
	Block[ {alpha, dim, mu, nu, a, b, gl3v},
		dim   = OptionValue[Dimension];
		{mu,nu} = Map[LorentzIndex[#, dim]&, {mui,nui}];
		{a,b}  = Map[SUNIndex[#]&, {ai,bi}];
		gl4v   = I OptionValue[CouplingConstant]^2 (DOT[SUNT[a] , SUNT[b] + SUNT[b] , SUNT[a]]) Pair[mu, nu];
		gl4v
	];

FCPrint[1,"ScalarGluonVertex.m loaded"];
End[]
