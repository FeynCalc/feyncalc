(* ::Package:: *)



(* :Title: PaVeReduce														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Reduction of Passarino-Veltman coefficient functions into
				Passarino Veltman scalar functions							*)

(* ------------------------------------------------------------------------ *)

PaVeReduce::usage =
"PaVeReduce[expr] reduces all Passarino-Veltman integrals (i.e. all PaVe's) in
expr down to scalar A0, B0, C0 and D0.";

Begin["`Package`"]
End[]

Begin["`PaVeReduce`Private`"]

pvrVerbose::usage="";
breduce::usage="";
a0tob0::usage="";
maxIterations::usage="";

Options[PaVeReduce ] = {
	A0ToB0			-> False,
	BReduce 		-> False,
	Collecting 		-> True,
	Dimension 		-> True,
	FCE 			-> True,
	FCVerbose 		-> False,
	Factoring 		-> Factor2,
	IsolateNames	-> False,
	Mandelstam		-> {},
	MaxIterations 	-> Infinity,
	PaVeAutoReduce 	-> False,
	PaVeOrderList 	-> {},
	WriteOutPaVe 	-> False
};

PaVeReduce[x_, opts:OptionsPattern[]] :=
	Block[ {op, wriout, nnx = x, res, time},

		op				= Join[FilterRules[Options[PaVeReduce], Except[{opts}]], {opts}];
		wriout 			= OptionValue[WriteOutPaVe];
		breduce 		= OptionValue[BReduce];
		a0tob0 			= OptionValue[A0ToB0];
		maxIterations	= OptionValue[MaxIterations];

		If [OptionValue[FCVerbose]===False,
			pvrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				pvrVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"PaVeReduce: Entering with: ", nnx, FCDoControl->pvrVerbose];


		If[ !FreeQ[nnx, StandardMatrixElement],
			nnx = Expand2[nnx, StandardMatrixElement];
		];


		FCPrint[1,"PaVeReduce: Starting the reduction.", FCDoControl->pvrVerbose];
		time=AbsoluteTime[];
		If[ StringQ[wriout] && (Head[x] === PaVe),
			res  = pavitp@@Join[{nnx, wriout}, op],
			res = pavereduce[nnx, op]
		];
		FCPrint[1,"PaVeReduce: Reduction done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pvrVerbose];
		FCPrint[3,"PaVeReduce: After the reduction: ", res, FCDoControl->pvrVerbose];


		If[ OptionValue[PaVeAutoReduce],
			FCPrint[1,"PaVeReduce: Setting the PaVeAutoReduce option to all PaVe-functions", FCDoControl->pvrVerbose];
			res = res /. PaVe[a__,b_List,c_List,ops___]:> PaVe[a,b,c,PaVeAutoReduce->True,ops]
		];
		(*
		If[ OptionValue[BReduce],
			FCPrint[1,"PaVeReduce: Setting the BReduce option to all B-functions", FCDoControl->pvrVerbose];
			res = res /. (h:B0|B00|B1|B11)[a_,b_,c_,ops___]:> h[a,b,c,BReduce->True,ops]
		];*)

		If[	OptionValue[Collecting],
			FCPrint[1,"PaVeReduce: Applying Collect2.", FCDoControl->pvrVerbose];
			time=AbsoluteTime[];
			res = Collect2[res, PaVeHeadsList,Factoring->OptionValue[Factoring]];
			FCPrint[1,"PaVeReduce: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pvrVerbose];
			FCPrint[3,"PaVeReduce: After Collect2: ", res, FCDoControl->pvrVerbose]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3,"PaVeReduce: Leaving.", FCDoControl->pvrVerbose];
		FCPrint[3,"PaVeReduce: Leaving with: ", res, FCDoControl->pvrVerbose];



		res
	];

(* These are the ultimate formulas for the reduction of coefficient
	functions. The reference is: Techniques for the calculation of
	electroweak radiative correctinos at the one-loop level and ...,
	by A. Denner (slightly rewritten by R.M),
	to appear in Fortschritte der Physik, 1993
*)
(* ***************************************************************** *)
(* Notation :    pij = (pi - pj)^2;  where p0 = (0,0,0,0), i.e.,
								p10 = p1^2, etc.  *)
(* ***************************************************************** *)
PaVeBr[i__, p_List, m_List, OptionsPattern[]] :=
	tT[Length[m]][i][Join[p, m]];

drop[] =
	{}; (* i.e. no index is an empty list *)
drop[x__] :=
	Drop[{x},-1];

(* A Kronecker delta *)
delt =
	If[ #1 === #2,
		1,
		0
	]&;
(* ***************************************************************** *)
(*                          pave21                                   *)
(* ***************************************************************** *)

(* This is only valid for UV - Divergences !! *)
$epsilon /:
	$epsilon^_Integer?Positive :=
		0;
$epsilon /:
	$epsilon A0[mm_] :=
		2 mm;
$epsilon /:
	$epsilon PaVe[0,{},{mm_}, OptionsPattern[]] :=
		2 mm;

$epsilon /:
	$epsilon B0[_, _, _] :=
		2;
$epsilon /:
	$epsilon PaVe[0,{_},{_,_}, OptionsPattern[]] :=
		2;

$epsilon /:
	$epsilon B1[_, _, _] :=
		-1;
$epsilon /:
	$epsilon PaVe[1,{_},{_,_}, OptionsPattern[]] :=
		-1;

$epsilon /:
	$epsilon C0[__] :=
		0;

$epsilon /:
	$epsilon PaVe[0,{_,_,_},{_,_,_}, OptionsPattern[]] :=
		0;

$epsilon /:
	$epsilon D0[__] :=
		0;

$epsilon /:
	$epsilon PaVe[0,{_,_,_,_,_,_},{_,_,_,_}, OptionsPattern[]] :=
		0;

$epsilon /:
	$epsilon T[3][1][_] :=
		0;
$epsilon /:
	$epsilon T[3][2][_] :=
		0;
$epsilon /:
	$epsilon T[4][ij__][_] :=
		0 /; Length[{ij}] < 4;

(* Things with head SmallVariable are discarded in this function *)
SetAttributes[demon, Listable];
HoldPattern[demon[demon[x_]]] :=
	demon[x];

null[_] :=
	0;
demon[0] =
	0;
demon[x_] :=
	MemSet[ demon[x],
		Block[	{nx = x, den}, (* This is quite tricky ... *)
				den =  Factor2[Denominator[nx]];
				If[ Head[den]=== Plus,
					nx = nx /. SmallVariable->null,
					(* Now:if there is something SmallVariable in the numerator*)
					nx = Factor2[nx] /. SmallVariable->null
				];
				Factor2[Numerator[nx]]/ Factor2[Denominator[nx]]
		]
	];

(* Calculate determinants only once. *)
det[x_List] :=
	det[x] = demon[ demon[Det[demon[x]]] // Factor2 ];

(* Remember:  pij = (pi - pj)^2, i.e, p1.p2 = 1/2 ( p10+p20-p12 ),
				with  p0 = (0,0,0,0) *)

(* {p10_, p12_, p20_, m02_,m12_,m22_} *)
kinmainv[2, {p10_, p12_, p20_, _, _, _}] :=
	1/demon[ p10 p20 -  (1/2 (p10+p20-p12) )^2 ] demon[{ {p20, -1/2 (p10+p20-p12)}, {-1/2 (p10+p20-p12), p10} }];

(* {p10_, p12_, p23_, p30_, p20_, p13_,m02_, m12_, m22_, m32_} *)
kinmainv[3, {p10_, p12_, p23_, p30_, p20_, p13_, _, _, _, _}] :=
	Block[ {p1p2, p1p3, p2p3},
		{p1p2, p1p3, p2p3} = demon[{p10-p12+p20, p10-p13+p30, p20-p23+p30}/2];
		1/det[{ {p10, p1p2, p1p3}, {p1p2, p20, p2p3}, {p1p3, p2p3, p30} }] *
		demon[ {{-p2p3^2 + p20*p30, p1p3*p2p3 - p1p2*p30, p1p2*p2p3 - p1p3*p20},
				{p1p3*p2p3 - p1p2*p30, -p1p3^2 + p10*p30, p1p2*p1p3 - p10*p2p3},
				{p1p2*p2p3 - p1p3*p20, p1p2*p1p3 - p10*p2p3, -p1p2^2 + p10*p20}}]
	];

Xinv[1][1,1][{pp_,_,_}] :=
	1/pp;

Xinv[2][i_, j_][a_] :=
	Xinv[2][i,j][a] = kinmainv[2, a][[i,j]];

Xinv[3][i_, j_][a_] :=
	Xinv[3][i,j][a] = kinmainv[3, a][[i,j]];

(* we put the SmallVariable - demon here *)
f[1][{pp_, m02_,m12_}] :=
	demon[ pp - m12 + m02 ];

(* {p10_,p12_,p20_, m02_,m12_,m22_}	*)
f[1][{p10_, _, _, m02_, m12_, _}] :=
	demon[ p10 - m12 + m02 ];

f[2][{_, _, p20_, m02_, _, m22_}] :=
	demon[ p20 - m22 + m02 ];
(* {p10_, p12_, p23_, p03_, p20_, p13_, m02_, m12_, m22_, m32_} *)
f[1][{p10_, _, _, _, _, _, m02_, m12_, _, _}] :=
	demon[ p10 - m12 + m02 ];

f[2][{_, _, _, _, p20_, _, m02_, _, m22_, _}] :=
	demon[ p20 - m22 + m02 ];

f[3][{_, _, _, p03_, _, _, m02_, _, _, m32_}] :=
	demon[ p03 - m32 + m02 ];

T[0][][___] :=
	0; (* in dimensional regularization *)

T[1][][{mm_}] :=
	PaVe[0,{},{mm}];

T[2][][{pp_, m12_, m22_}] :=
	PaVe[0,{pp},{m12,m22}];

T[3][][{p10_,p12_,p20_, m02_,m12_,m22_}] :=
	PaVe[0,{p10,p12,p20},{m02,m12,m22}];

T[4][][{p10_, p12_, p23_, p03_, p20_, p13_, m02_, m12_, m22_, m32_}] :=
	PaVe[0,{p10,p12,p23,p03,p20,p13},{m02,m12,m22,m32}];

(* The translated argument lists obtained by canceling  *)

(* {pp_, m12_, m22_} *)

c[0][{_, _, m22_}] :=
	{m22};

c[1][{_, m12_, _}] :=
	{m12};

(* {p10_,p12_,p20_, m02_,m12_,m22_} *)
c[0][{_, p12_, _, _, m12_,m22_}] :=
	{p12, m22, m12};

c[1][{_, _, p20_, m02_, _, m22_}] :=
	{p20, m02, m22};

c[2][{p10_, _, _, m02_, m12_, _}] :=
	{p10, m02, m12};

(* {p10_, p12_, p23_, p03_, p20_, p13_, m02_, m12_, m22_, m32_} *)
c[0][{_, p12_, p23_, _, _, p13_, _, m12_, m22_, m32_}] :=
	{p13, p12, p23, m32, m12, m22};

c[1][{_, _, p23_, p03_, p20_, _, m02_, _, m22_, m32_}] :=
	{p20, p23, p03, m02, m22, m32};

c[2][{p10_, _, _, p03_, _, p13_, m02_, m12_, _, m32_}] :=
	{p10, p13, p03, m02, m12, m32};

c[3][{p10_, p12_, _, _, p20_, _, m02_, m12_, m22_, _}] :=
	{p10, p12, p20, m02, m12, m22};

(* getmdef *)
getm[{_}] :=
	0;

getm[{_,_,_}] :=
	1;

getm[{_,_,_,_,_,_}] :=
	2;

getm[{_,_,_,_,_,_,_,_,_,_}] :=
	3

pluep2[x__] :=
	Plus[x]/;!FreeQ2[{x}, {tT,B0,B1,B00,B11,C0,D0,T}];

(* 	equation  (4.18) from arXiv:0709.1075, handles T integrals with at least two 0 indices *)
tT[N_Integer][0,0, i___Integer][a_List] :=
	Block[ {P, M, k, epsi,tmp },
		FCPrint[3,"PaVeReduce: tT: Entering: $LimitTo4=True, N<3",FCDoControl->pvrVerbose];
		P = 2 + Length[{i}];
		M = getm[a];
		tmp = 1/(2 + P - M) (R[N, 0, 0][i][a] - Sum[ R[N, k][k, i][a], {k, M}])+
		Expand[( 1/(2 + P - M) epsi/(2 + P - M) (R[N, 0, 0][i][a] - Sum[ R[N, k][k, i][a], {k, M}])
			)/.Plus->pluep2]/.epsi->$epsilon/.pluep2->Plus;
		FCPrint[3,"PaVeReduce: tT: leaving with ",tmp,FCDoControl->pvrVerbose];
		tmp
		] /; $LimitTo4 && N<3;

tT[N_Integer][0,0, i___Integer][a_List] :=
	Block[ {P, M, k, epsi,tmp},
		FCPrint[3,"PaVeReduce: tT: Entering: $LimitTo4=True, N>3, $LimitTo4IRUnsafe=True",FCDoControl->pvrVerbose];
		P = 2 + Length[{i}];
		M = getm[a];
		tmp = 1/(2 + P - M) (R[N, 0, 0][i][a] - Sum[ R[N, k][k, i][a], {k, M}])+
		Expand[( 1/(2 + P - M) epsi/(2 + P - M) (R[N, 0, 0][i][a] - Sum[ R[N, k][k, i][a], {k, M}])
			)/.Plus->pluep2]/.epsi->$epsilon/.pluep2->Plus;
		FCPrint[3,"PaVeReduce: tT: leaving with ",tmp,FCDoControl->pvrVerbose];
		tmp
		] /; $LimitTo4 && N>=3 && $LimitTo4IRUnsafe;

(* $dIM gets defined from the option of PaVeReduce *)
tT[N_Integer][0,0, i___Integer][a_List] :=
	Block[ {P, M, k, epsi, tmpR, tmp },
		P = 2 + Length[{i}];
		M = getm[a];
		tmp = 1/($dIM + P -2 - M) (tmpR[N, 0, 0][i][a] - Sum[ tmpR[N, k][k, i][a], {k, M}]);
		FCPrint[3,"PaVeReduce: tT: leaving with ",tmp,FCDoControl->pvrVerbose];
		tmp/.tmpR->R

	] /; $LimitTo4 =!= True || ($LimitTo4 && N>=3 && !$LimitTo4IRUnsafe);

(* two special cases *)
R[n0__][j__][a_] :=
	(R[n0][Sequence @@ Sort[{j}]][a] ) /; !OrderedQ[{j}];

R[n0__][{}][a_] :=
	R[n0][][a];

T[n0__][j__][a_] :=
	(T[n0][Sequence @@ Sort[{j}]][a] ) /; !OrderedQ[{j}];

(*
tT[N][mu_1,mu_2,...,mu_p][{p_1,...,p_(N-1),m_0,...,m_(N-1)}] = c*\int q^mu_1 ... q^mu_p / (D0*D1*...*D(N-1))
with D0 = q^2-m^2, D1 = (q+p_i)-m_i^2, i=1,...,N-1

tT[N][mu_1,mu_2,...,mu_p][{p_1,...,p_(N-1),m_0,...,m_(N-1)}] = \Sum_{i_1,...,i_p=0}^{N-1} *
tT[N][i_1,...,i_p][{p_1,...,p_(N-1),m_0,...,m_(N-1)}]* p_{i_1 mu_1} * ... * p_{i_p mu_p}
*)

(* A0 *)

tT[1][0][{SmallVariable[_]^_.}] :=
	0;

(* A0 to B0 *)

tT[1][0][{mm_}] :=
	mm PaVe[0,{0},{mm,mm}] + mm/; a0tob0 && $LimitTo4;

tT[1][0][{mm_}] :=
	2mm/($dIM-2) PaVe[0,{0},{mm,mm}] /; a0tob0 && !$LimitTo4;

(* Special cases of B functions with zero Gram determinant	*)

(* 	B1...(0,m1,m2) are not further reducible in terms of other PaVe functions *)
tT[2][inds:1..][{0,m1_,m2_}]:=
	PaVe[inds,{0},{m1,m2}]/; m1=!=m2;

(* 	B1....(0,m,m) can be reduced further *)
tT[2][inds:1..][{0,m_,m_}]:=
	(-1)^Length[{inds}]/(1+Length[{inds}]) PaVe[0,{0},{m,m}];

(* Special cases of B0 scalar functions	*)

tT[2][0][{kl_, kmm_, mm:Except[_SmallVariable | 0]}]:=
	(PaVe[0,{},{mm}]/mm)/; breduce && (({kl,kmm}/.SmallVariable[_]->0) === {0,0});

tT[2][0][{kl_, mm:Except[_SmallVariable | 0], kmm_}]:=
	(PaVe[0,{},{mm}]/mm)/; breduce && (({kl,kmm}/.SmallVariable[_]->0) === {0,0});

tT[2][0][{0, m1_, m2_}]:=
	(1/(m1-m2) PaVe[0,{},{m1}] - 1/(m1-m2) PaVe[0,{},{m2}])/; (m1 =!= m2) && breduce;

tT[2][0][{0, mm:Except[_SmallVariable | 0], mm:Except[_SmallVariable | 0]}]:=
	(PaVe[0,{},{mm}]/mm - 1)/; breduce && $LimitTo4;

tT[2][0][{0, mm:Except[_SmallVariable | 0], mm:Except[_SmallVariable | 0]}]:=
	(($dIM-2)*PaVe[0,{},{mm}]/(2mm))/; breduce && !$LimitTo4;


(*
tT[2][0,0][{0, mm:Except[_SmallVariable | 0], mm:Except[_SmallVariable | 0]}]:=
	(PaVe[0,{},{mm}]/mm - 1)/; breduce && $LimitTo4;


b00[0, mm:Except[_SmallVariable | 0], mm:Except[_SmallVariable | 0]] :=
	mm / 2 ( B0[0,mm,mm] + 1 );

b00[SmallVariable[em_]^n_., mm:Except[_SmallVariable | 0], mm:Except[_SmallVariable | 0]] :=
	mm / 2 ( B0[em^n,mm,mm] + 1 );
*)














(* General case, T integrals with zero Gram determinant are not evaluated	*)
tT[N_Integer][k_Integer,i___Integer][a_List] :=
	Block[ {P, M ,r, kp, tmp,tmpXinv,tmpR,tmpT },
		FCPrint[3,"PaVeReduce: tT: Entering with N and a: ",N," ",a,FCDoControl->pvrVerbose];
		FCPrint[3,"PaVeReduce: tT: Gram determinant: ",gramDet[Drop[a,-N]],FCDoControl->pvrVerbose];
		P = 1 + Length[{i}];
		M = getm[a];
		tmp = Sum[ Xinv[M][k, kp][a]  ( R[N,kp][i][a] - Sum[ delt[ kp,{i}[[r]] ] (T[N]@@Join[{0,0}, Delete[{i},r]])[a], {r, P-1}]), {kp, M}];
		FCPrint[3,"PaVeReduce: tT: leaving with ",tmp,FCDoControl->pvrVerbose];
		tmp/.tmpR->R/.tmpT->T/.tmpXinv->Xinv
	]/; (gramDet[Drop[a,-N]]=!=0 || N===0) && k=!=0;

(* no M's in i *)
R[N_Integer, 0, 0][i___Integer][a_List] :=
	Block[ {q,P,M,tmpT,tmp},
		q = Length[{i}];
		P = 2 + q;
		M = getm[a];
		tmp = demon[a[[-N]]] tmpT[N][i][a]  + tmpT[N-1][i][ c[0][a] ];
		FCPrint[3,"PaVeReduce: R: leaving with ",tmp,FCDoControl->pvrVerbose];
		tmp /. tmpT-> T
	] /; FreeQ[{i}, getm[a]];

R[N_Integer,0,0][i___Integer, mm:(_Integer)..][a_List] :=
	Block[ {q,M,P,j,k,tmp,tmpT},
		q = Length[{i}];
		M = getm[a];
		P = Length[{mm}] + 2 + q;
		tmp = demon[ a[[-N]] ] T[N][i, mm][a] +
		(* here was the tough bug found by Ralph Schuster ... *)
		(-1)^(P - q) ( tmpT[N - 1][i][c[0][a]] + Sum[
		Binomial[P - 2 - q, j] * Sum @@ Prepend[ Array[List[k[#], M - 1]&, j],
		(tmpT[N-1]@@Join[{i}, Array[k,j]])[c[0][a]]], {j,P - 2 - q}]);
		FCPrint[3,"PaVeReduce: R: leaving with ",tmp,FCDoControl->pvrVerbose];
		tmp /. tmpT-> T

	] /; ({mm}[[1]] === getm[a]);

(* 4.19 , no M's*)
R[N_Integer, k_Integer][i___Integer][a_List] :=
	Block[ {q,P,M,tmp ,tmpT},
		q = Length[{i}];
		P = 1 + q;
		M = getm[a];
		tmp = 1/2( (tmpT[N - 1] @@ til[i][k])[ c[k][a] ] theta[k, i] - f[k][a] tmpT[N][i][a] - tmpT[N - 1][i][c[0][a]]);
		FCPrint[3,"PaVeReduce: R: leaving with ",tmp,FCDoControl->pvrVerbose];
		tmp /. tmpT-> T

	]/; FreeQ[{i}, getm[a]];

R[N_Integer,k_Integer][i___Integer, mm:(_Integer)..][a_List] :=
	Block[ {q, P, M, kk, j, tmp, tmpT},
		q = Length[{i}];
		P = Length[{mm}] + 1 + q;
		M = getm[a];
		tmp = 1/2( (tmpT[N-1] @@ til[i,mm][k])[c[k][a]] theta[k, i, mm] -
		f[k][a] tmpT[N][i,mm][a] -(-1)^(P - 1 - q) (tmpT[N - 1][i][c[0][a]] +
		Sum[Binomial[P - 1 - q, j]  Sum@@Prepend[Array[List[kk[#], M -1 ]&,j], (tmpT[N - 1]@@Join[{i},
		Array[kk, j]])[c[0][a]]], {j, P - 1 - q} ]));
		FCPrint[3,"PaVeReduce: R: leaving with ",tmp,FCDoControl->pvrVerbose];
		tmp /. tmpT-> T

	] /;({mm}[[1]] === getm[a]);

(* 4.20 *)
(* thetadef *)
theta[k_Integer, i___Integer] :=
	1 /; FreeQ[{i}, k];

theta[k_Integer, i___Integer] :=
	0 /;!FreeQ[{i}, k];

tm[a_Integer, b_Integer] :=
	a /; a<=b;

tm[a_Integer, b_Integer] :=
	(a-1) /; a > b;

til[][_] = {};

til[x__][k_] :=
	Map[tm[#,k]&, {x}];

(* Decomposition down to scalar integrals *)
(* ****************************************************************** *)

cancel[x_] :=
	Cancel[x/.Plus->pll]/.pll->Plus;

pavitp[xXX_PaVe, dir_, opts:OptionsPattern[]] :=
	Block[ {nx, file, temp, set,xxx,a,abbs,abbstr,dir1},

		paV[xy__, p_List, m_List, paveopts:OptionsPattern[]] :=
			PaVe[xy,C,p,C,m,paveopts];
		xxx = paV@@xXX;

		(*Changed 18/9-2000, F.Orellana*)
		abbs = DownValues[Abbreviation] /. Abbreviation -> Identity /. HoldPattern -> Identity;

		nx = StringReplace[ ToString[InputForm[xxx/.abbs], PageWidth -> 222], $Abbreviations];
		nx = StringJoin[dir, nx, ".s"];

		FCPrint[2,"nx  =", nx,FCDoControl->pvrVerbose];
		If[ Streams[nx] === {},
			(*Mac fix, 18/9-2000, F.Orellana*)
			file = FileType[nx];

			If[ file === File,
				FCPrint[2,"file  =", file];
				temp = (Get@@{nx})//PaVeOrder;
				(* If something went wrong in writing the file *)
				If[ Head[temp]=!=Plus,
					file = {}
				]
			];

			If[ file =!= File && file =!= Directory,
				temp = FixedPoint[ReleaseHold, PaVeReduce[xXX, WriteOutPaVe->False,opts]]//PaVeOrder;
				FCPrint[2,"writing result to ",nx,FCDoControl->pvrVerbose];
				OpenWrite @@ {nx, FormatType -> InputForm };
				WriteString @@ {nx, "( "};
				Write @@ {nx, temp};
				WriteString @@ {nx, "  ) "};
				Close @@ {nx};
			],
			temp = PaVeReduce[xXX, WriteOutPaVe->False,opts]//PaVeOrder;
		];
		temp
	];

(*
pavereduce[0,___]:=0;
pavereduce[w_,___]:=w/;NTerms[w]===1 && FreeQ[w,PaVe];

(*
pavereduce[ a_ b_,ops___ ]:=cancel[ a pavereduce[ b,ops] ]/;
													FreeQ[a,PaVe]&&!FreeQ[a,StandardMatrixElement];
*)

pavereduce[a_Times, ops___] :=
	cancel[ SelectFree[SelectNotFree[a,StandardMatrixElement],PaVe] *
pavereduce[a/SelectFree[SelectNotFree[a,StandardMatrixElement],PaVe]
				]  ] /;
		SelectFree[SelectNotFree[a,StandardMatrixElement],PaVe] =!= 1;
*)


(* ********************************************************************** *)

(* This default setting of Dimension results --- together with
	$LimitTo4 = True --- into dimensional regularization
	(for the ultraviolett divergencies ) with
	the limit Dimension -> 4 being taken.
	If the option Dimension is set to some explicit variable (d for instance),
	no limit is taken and d occurs in the result.
*)

(* Gram determinant for a 2-point function	*)
gramDet[{p10_}]:=
	gramDet[{p10}] =
		Factor2[2 p10]

(* Gram determinant for a 3-point function	*)
gramDet[{p10_, p12_, p20_}]:=
	gramDet[{p10, p12, p20}] =
		Factor2[4 p10 p20 -  ((p10+p20-p12))^2];

(* Gram determinant for a 4-point function	*)
gramDet[{p10_, p12_, p23_, p30_, p20_, p13_}]:=
	gramDet[{p10, p12, p23, p30, p20, p13}]=
		Factor2[2 (4 p10 p20 p30 - (p10 - p12 + p20)^2 p30 -
			p20 (p10 - p13 + p30)^2 + (p10 - p12 + p20) (p10 - p13 +
			p30) (p20 - p23 + p30) - p10 (p20 - p23 + p30)^2)];

pavereduce[exp_, OptionsPattern[]] :=
	exp /; FreeQ2[exp, PaVeHeadsList];

pavereduce[w_ , opts:OptionsPattern[]] :=
	Block[	{mpa,nw,nn,pre,re = 0},

			FCPrint[3,"PaVeReduce: pavereduce: Entering with: ", w, FCDoControl->pvrVerbose];
			mpa = w/. StandardMatrixElement[__]->0;
			nw = w - mpa;
			re = pavereduce[mpa, opts];
			If[ Head[nw] === Plus,
				nn = Length[nw],
				nn = 1
			];
			For[i = 1, i<=nn, i++,
				FCPrint[2,"breaking down # ", i, " / ", nn];
				If[ nn===1,
					pre = PartitHead[nw,StandardMatrixElement],
					pre = PartitHead[nw[[i]],StandardMatrixElement]
				];
				re = re + pre[[2]] pavereduce[ pre[[1]], opts]
			];
		re
	]/; !FreeQ[w, StandardMatrixElement];

pavereduce[pvli_List, opts:OptionsPattern[]] :=
	Block[ {i,set,le = Length[pvli],npvli},
		npvli = {};
		FCPrint[3,"PaVeReduce: pavereduce: Entering with: ", pvli, FCDoControl->pvrVerbose];
		Do[ FCPrint[2," Working with # ",i," out of ",le];
			npvli = Append[npvli, pavereduce[pvli[[i]],opts]],
			{i,le}
		];
		npvli
	];

pavereduce[brex_, opts:OptionsPattern[]] :=
	Block[	{	t, mand, result, ij, tri, kkk, trick, dimen, msu, isok,
				is, pl2, paveorderli, breakx, cofun2,cofun, tvarS, paveProtect},

			FCPrint[3,"PaVeReduce: pavereduce: Entering with: ", brex, FCDoControl->pvrVerbose];


			paveorderli = OptionValue[PaVeReduce,{opts}, PaVeOrderList];
			mand = OptionValue[PaVeReduce,{opts}, Mandelstam];
			isok = OptionValue[PaVeReduce,{opts}, IsolateNames];
			dimen = OptionValue[PaVeReduce,{opts}, Dimension];


			If[ (dimen =!= True) && ($LimitTo4 =!= True),
				$dIM = dimen,
				$dIM = D
			];

			If[ !FreeQ[ brex, PaVe],
				breakx = Collect[brex, PaVe[__]],
				breakx = brex
			];

			isolateP[x__] :=
				Isolate[x, IsolateNames -> isok];

			tri[x_  y_] :=
				tri[x] tri[y];

			tri[a_ x_] :=
				( a tri[x] )/;FreeQ[a,Plus] || Head[a]===PaVe;

			tri[a_ ] :=
				a /;FreeQ[a,Plus] || Head[a]===PaVe;

			mand = mand /. SmallVariable->Identity;

			If[ mand==={},
				If[ ($LimitTo4 === False ) && (Head[brex] === PaVe),
					tvarS = Variables[ Join @@ Take[(brex /.
						PaVe[x__, y_List, OptionsPattern[]] :> PaVe[x, y]), -2] ];
					trick[z_] :=
						Collect2[z, tvarS],
					trick[z_] :=
						z
				],
				trick[z_] :=
					trick[z] = TrickMandelstam[z,mand]//Factor2
			];

			msu = {};

			pl2[x__] :=
				kkk[ Plus[x] ]/; FreeQ2[{x}, {A0,B0,B1,B00,B11,C0,D0,PaVe}];

			backpc[a_,b_,c_,d_,e_,f_] :=
				PaVeOrder[C0[a,b,c,d,e,f], PaVeOrderList -> paveorderli];

			backpd[a_] :=
				D0[a];

			backpd[a_,b_,c_,d_,e_,f_,  v_,w_,x_,y_] :=
				PaVeOrder[ D0[a,b,c,d,e,f,v,w,x,y], PaVeOrderList -> paveorderli];

			pluep[y__] :=
				Plus[y]/;!FreeQ2[{y}, {$epsilon,A0,B0,B1,B00,B11}];

			FCPrint[3,"PaVeReduce: pavereduce: Staring reduction.", FCDoControl->pvrVerbose];
			breakx = breakx/.msu;

			(*	Currently, the reduction is not implemented for pentagons and other higher point function	*)
			breakx = breakx /. {
				PaVe[i_,j___,  m_List, l_List, o:OptionsPattern[]]/;
					MatchQ[{i,j}, {Integer___}] && Length[m]>6 :>
						paveProtect[i,j,  m, l, o]
			};

			breakx = ToPaVe2[breakx];

			If[ FreeQ[breakx,PaVe],
				t = breakx,
				t = FixedPoint[(#/.T->tT)&,breakx/.PaVe->PaVeBr, maxIterations]
			];

			FCPrint[3,"PaVeReduce: pavereduce: First stage done: ", t, FCDoControl->pvrVerbose];

			t = t/.C0->backpc/.D0->backpd/.paveProtect->PaVe;

			FCPrint[3,"PaVeReduce: pavereduce: Second stage done: ", t, FCDoControl->pvrVerbose];

			If[ !FreeQ[t, HoldForm],
				t = FRH[t, IsolateNames -> isok]
			];

			If[	!FreeQ2[t,{T,tT}],
				t = t//. (T|tT)[n_Integer][i__][{a__}] :> PaVe[i,Drop[{a},-n],Take[{a},-n]]
			];

			t = Expand2[t,Join[PaVeHeadsList,{$epsilon}]];

			FCPrint[3,"PaVeReduce: pavereduce: Third stage done: ", t, FCDoControl->pvrVerbose];

			t = Collect2[t,Join[PaVeHeadsList,{HoldPattern,DOT}],Factoring->Factor2];

			FCPrint[3,"PaVeReduce: pavereduce: After Collect2: ", t, FCDoControl->pvrVerbose];
(*
			t = Collect[t, {A0[__],B0[__], B1[__], B00[__], B11[__], C0[__], D0[__], PaVe[__],
							HoldPattern[Dot[__]], HoldPattern[DOT[__]]}, Factor2];
*)
			If[ !FreeQ[t, $epsilon],
				t = Expand[t/.Plus->pluep]/.$epsilon->0/.pluep->Plus
			];

			result = t;

			(* get the "linear" part *)
			FCPrint[2,"check4 ", MemoryInUse[]," MB used"];

			cofun[0] = 0;
			cofun[a_ b_] :=
				a cofun[b]/;!FreeQ2[a, {A0,B0,B1,B00,B11,C0,D0,PaVe} ];

			cofun2[0] = 0;

			cofun2[y_] :=
				y/;FreeQ[y,Plus];

			cofun2[y_] :=
				trick[y];


			If[ isok=!=False && Head[result]=!=PaVe,
				result = cofun/@( result + nuLL );

				isolatefirst[a_ b_] :=
					(a isolatefirst[b])/; FreeQ2[a,{A0,B0,B1,B00,B11,C0,D0,PaVe}];

				isolatefirst[a_] :=
					a /; FreeQ2[a,{A0,B0,B1,B00,B11,C0,D0,PaVe}];

				result = isolatefirst /@ result;
				isolatetri[a_] :=
					isolateP[ trick[a] ];

				result = (result/.nuLL->0)/.isolatefirst->isolatetri/.cofun->cofun2
			];

			FCPrint[3,"PaVeReduce: pavereduce: After isolations: ", result, FCDoControl->pvrVerbose];

			If[ isok=!=False,
				result = isolateP[ result ],
				result = trick /@ result
			];

			FCPrint[3,"PaVeReduce: pavereduce: Leaving with: ", result, FCDoControl->pvrVerbose];

			result
	]/;FreeQ[brex,StandardMatrixElement];

FCPrint[1,"PaVeReduce.m loaded."];
End[]
