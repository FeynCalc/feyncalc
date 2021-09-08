(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PaVeIntegrals													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Direct Passarino-Veltman integrals and their derivatives		*)

(* ------------------------------------------------------------------------ *)


A0::usage =
"A0[m^2] is the Passarino-Veltman one-point integral $A_0.$.";

A00::usage =
"A00[m^2] is the Passarino-Veltman coefficient function $A_{00}$, i.e. the
coefficient function multiplying $g^{\\mu \\nu}$. The argument  is a scalar and
has mass dimension 2.";

A0ToB0::usage =
"A0ToB0 is an option for A0. If set to True, A0[m^2] is expressed by (1 + B0[0,
m^2, m^2]) m^2.";

B0::usage =
"B0[pp, ma^2, mb^2] is the Passarino-Veltman two-point integral $B_0$. All
arguments are scalars and have dimension mass squared. If the option BReduce
is set to True, certain B0's are reduced to A0's. Setting the option B0Unique
to True simplifies B0[a,0,a] and B0[0,0,a].";

B0Real::usage =
"B0Real is an option of B0 (default False). If set to True, B0 is assumed to be
real and the relation B0[a,0,a] = 2 + B0[0,a,a] is applied.";

B0Unique::usage =
"B0Unique is an option of B0. If set to True, B0[0,0,m2] is replaced with
(B0[0,m2,m2]+1) and B0[m2,0,m2] simplifies to (B0[0,m2,m2]+2).";

B00::usage =
"B00[pp, ma^2, mb^2] is the Passarino-Veltman $B_{00}$-function, i.e., the
coefficient function of the metric tensor. All arguments are scalars and have
dimension mass squared.";

B1::usage =
"B1[pp, ma^2, mb^2] the Passarino-Veltman $B_1$-function. All arguments are
scalars and have dimension mass squared.";

B11::usage =
"B11[pp, ma^2, mb^2] is the Passarino-Veltman $B_{11}$-function, i.e. the
coefficient function of $p^{\\mu } p^{\\nu }$. All arguments are scalars and
have dimension mass squared.";

BReduce::usage =
"BReduce is an option for B0, B00, B1, B11 determining whether reductions to A0
and B0 will be done.";

C0::usage =
"C0[p10, p12, p20, m1^2, m2^2, m3^2] is the scalar Passarino-Veltman $C_0$
function. The convention for the arguments is that if the denominator of the
integrand has the form $([q^2-m1^2] [(q+p1)^2-m2^2] [(q+p2)^2-m3^2])$, the
first three arguments of C0 are the scalar products $p10 = p1^2$, $p12 =
(p1-p2).(p1-p2)$, $p20 = p2^2$.";

D0::usage =
"D0[p10, p12, p23, p30, p20, p13, m1^2, m2^2, m3^2, m4^2 ] is the
Passarino-Veltman $D_0$ function. The convention for the arguments is that if
the denominator of the integrand has the form $([q^2-m1^2] [(q+p1)^2-m2^2]
[(q+p2)^2-m3^2] [(q+p3)^2-m4^2])$, the first six arguments of D0 are the
scalar products $p10 = p1^2$, $p12 = (p1-p2)^2$, $p23 = (p2-p3)^2$, $p30 =
p3^2$, $p20 = p2^2$, $p13 = (p1-p3)^2$.";

DB0::usage =
"DB0[p2, m1^2, m2^2] is the derivative of the two-point function B0[p2, m1^2,
m2^2] with respect to p2.";

DB1::usage =
"DB1[p2, m1^2, m2^2] is the derivative of B1[p2,m1^2,m2^2] with respect to p2.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

PaVeHeadsList;

End[]

Begin["`PaVeIntegrals`Private`"]

PaVeHeadsList  = {PaVe,GenPaVe,A0,A00,B0,B00,B1,B11,DB1,C0,D0};
ClearAttributes[B0, ReadProtected];
ClearAttributes[B00, ReadProtected];
ClearAttributes[B1, ReadProtected];
ClearAttributes[B11, ReadProtected];

Options[A0] = {
	A0ToB0 -> False
};

Options[B0] = {
	B0Real 		-> False,
	B0Unique	-> False,
	BReduce		-> False
};

Options[B00] = {
	BReduce	-> True
};

Options[B1] = {
	BReduce	-> True
};

Options[B11] = {
	BReduce	-> True
};

Options[DB1] = {
	BReduce	-> True
};

smanull[_] :=
	0;
smad[x_] :=
	Block[ {nx = Factor2[x]/.SmallVariable->smanull},
		Factor2[Numerator[nx]]/ Factor2[Denominator[nx]]
	];

A0[0,OptionsPattern[]]:=
	0;

A0[SmallVariable[_]^_. ,OptionsPattern[]]:=
	0;

A0[mm_,OptionsPattern[]] :=
	PaVeReduce[PaVe[0,{},{mm}],PaVeAutoReduce->True,A0ToB0->True]/; OptionValue[A0ToB0] &&
	!( BReduce/.Options[B0]) && FCPatternFreeQ[{mm},{SmallVariable}];

A0 /:
	MakeBoxes[A0[m_, OptionsPattern[]]  ,TraditionalForm] :=
		ToBoxes[HoldForm[PaVe[0,{},{m}]],TraditionalForm];

A00[0,OptionsPattern[]]:=
	0;

A00[SmallVariable[_]^_. ,OptionsPattern[]]:=
	0;

A00[mm_]:=
	(mm/4 A0[mm] + mm^2/8)/; $LimitTo4;

A00[mm_]:=
	(mm/D A0[mm]); !$LimitTo4;

A00 /:
	MakeBoxes[A00[m_, OptionsPattern[]]  ,TraditionalForm] :=
		ToBoxes[HoldForm[PaVe[0,0,{},{m}]],TraditionalForm];

B0[0,0,0, OptionsPattern[]]:=
	0/; !$KeepLogDivergentScalelessIntegrals;

(* ordering *)
B0[pe_,me2_,me1_, opt:OptionsPattern[]] :=
	B0 @@ Prepend[ {me1,me2,opt}, Expand[pe]] /; !OrderedQ[{me2,me1}];

(* generic rules *)
B0[SmallVariable[pp_]^j_., SmallVariable[a_]^n_., SmallVariable[b_]^m_., OptionsPattern[]] :=
	B0[pp^j, a^n, b^m];

B0[0, SmallVariable[a_]^n_., SmallVariable[b_]^m_.] :=
	B0[0, a^n, b^m];

(* special replacements *)
B0[0,0,mm:Except[_SmallVariable | 0], OptionsPattern[]] :=
	( B0[0,mm,mm] + 1 ) /; OptionValue[B0Unique];

B0[mm_,0,mm_,OptionsPattern[]] :=
	( B0[0,mm,mm] + 2)/; OptionValue[B0Unique] && OptionValue[B0Real] && mm=!=0;

(* further B0 reduction *)
B0[pp_,mm1_,mm2_,OptionsPattern[]] :=
	PaVeReduce[PaVe[0,{pp},{mm1,mm2}],BReduce->True,PaVeAutoReduce->True]/; OptionValue[BReduce] && FCPatternFreeQ[{pp,mm1,mm2}] &&
	(pp/.SmallVariable[_]->0)===0;

B0 /:
	MakeBoxes[B0[p10_,m02_,m12_, OptionsPattern[]]  ,TraditionalForm] :=
		ToBoxes[HoldForm[PaVe[0,{p10},{m02,m12}]],TraditionalForm];

B00[0,0,0, OptionsPattern[]]:=
	0/; !$KeepLogDivergentScalelessIntegrals;

B00[pp_,mm1_,mm2_, OptionsPattern[]] :=
	b00[pp,mm1,mm2] /; $LimitTo4 && OptionValue[BReduce] && FCPatternFreeQ[{pp,mm1,mm2}] && !MatchQ[{pp,mm1,mm2},{0,0,0}];

B00[pp_,mm1_,mm2_, OptionsPattern[]] :=
	PaVeReduce[PaVe[0,0,{pp},{mm1,mm2}],PaVeAutoReduce->True] /; !$LimitTo4 && OptionValue[BReduce] && FCPatternFreeQ[{pp,mm1,mm2}];

b00[0, SmallVariable[mm_], SmallVariable[mm_]] :=
	0;

b00[0, mm:Except[_SmallVariable | 0], mm:Except[_SmallVariable | 0]] :=
	mm / 2 ( B0[0,mm,mm] + 1 );

b00[SmallVariable[em_]^n_., mm:Except[_SmallVariable | 0], mm:Except[_SmallVariable | 0]] :=
	mm / 2 ( B0[em^n,mm,mm] + 1 );

b00[pp:Except[_SmallVariable | 0] , mm_, mm_] :=
	1/6 ( A0[mm]+B0[pp,mm,mm] smad[2 mm - pp/2] + smad[2 mm - pp/3]);

b00[pp_,mm1_,mm2_] :=
	( 1/6 ( A0[mm2]+ (B1[pp,mm1,mm2] ) smad[pp-mm2+mm1] )+ smad[mm1/3] B0[pp,mm1,mm2] +
	smad[ 1/6 ( mm1 + mm2 - pp/3 ) ] ) /; mm1=!=mm2;

B00 /:
	MakeBoxes[B00[p10_,m02_,m12_, OptionsPattern[]]  ,TraditionalForm] :=
		ToBoxes[HoldForm[PaVe[0,0,{p10},{m02,m12}]],TraditionalForm];

B1[pp_,mm1_,mm2_, OptionsPattern[]] :=
	b1[pp,mm1,mm2] /; $LimitTo4 && OptionValue[BReduce] && FCPatternFreeQ[{pp,mm1,mm2}] && !MatchQ[{pp,mm1,mm2},{0,0,0}];

B1[0,0,0, OptionsPattern[]] :=
	0/; !$KeepLogDivergentScalelessIntegrals;

(* General case for $LimitTo4=False; The case with zero momentum and different masses is exluded *)
B1[pp_,mm1_,mm2_, OptionsPattern[]] :=
	PaVeReduce[PaVe[1,{pp},{mm1,mm2}],PaVeAutoReduce->True] /;
		!$LimitTo4 && OptionValue[BReduce] && FCPatternFreeQ[{pp,mm1,mm2}] && !(pp===0 && (mm1=!=mm2));

(* Special cases, valid for $LimitTo4=True; *)

b1[SmallVariable[me_]^n_., SmallVariable[me_]^n_., SmallVariable[mla_]^_.] :=
	( -1/2 B0[SmallVariable[me]^n, SmallVariable[me]^n, 0] - 1/2 )/; TrueQ[mla < me];

b1[SmallVariable[me_]^n_., SmallVariable[mla_]^n_., SmallVariable[me_]^_.] :=
	(1/2 - 1/2 B0[SmallVariable[me]^n,0 ,SmallVariable[me]^n]) /; TrueQ[mla < me];


b1[0,mm_,mm_] :=
	-1/2 B0[0,mm,mm];

b1[mm_, mm_, 0] :=
	-1/2 B0[mm, mm, 0] - 1/2;

b1[mm_, 0, mm_] :=
	1/2 - B0[mm,0,mm]/2;

b1[0,0,mm_] :=
	-1/2 B0[0,0,mm]+1/4;

b1[SmallVariable[_]^_.,0, mm:Except[_SmallVariable | 0]] :=
	( -1/2 B0[0,0,mm] + 1/4 );

b1[0,SmallVariable[_]^_., mm:Except[_SmallVariable | 0]] :=
	( -1/2 B0[0,0,mm] + 1/4 );

b1[0, mm:Except[_SmallVariable | 0], 0] :=
	( -1/2 B0[0,0,mm] - 1/4 );

b1[SmallVariable[_]^n_.,SmallVariable[_]^n_., mm:Except[_SmallVariable | 0]] :=
	( -1/2 B0[0,0,mm] + 1/4 );

b1[SmallVariable[_]^n_.,mm:Except[_SmallVariable | 0], SmallVariable[_]^n_.] :=
	( -1/2 B0[0,0,mm] - 1/4 );

b1[SmallVariable[_]^_.,mm:Except[_SmallVariable | 0], 0] :=
	( -1/2 B0[0,0,mm] - 1/4 );

b1[pp:Except[_SmallVariable | 0], ma0_, ma1_] :=
	(smad[ma1-ma0]/(2 pp) (B0[pp,ma0,ma1] -	B0[0,ma0,ma1]) - 1/2 B0[pp,ma0,ma1]);

B1 /:
	MakeBoxes[B1[p10_,m02_,m12_, OptionsPattern[]]  ,TraditionalForm] :=
		ToBoxes[HoldForm[PaVe[1,{p10},{m02,m12}]],TraditionalForm];

B11[0,0,0, OptionsPattern[]] :=
	0/; !$KeepLogDivergentScalelessIntegrals;

B11[pp_,mm1_,mm2_, OptionsPattern[]] :=
	b11[pp,mm1,mm2] /; $LimitTo4 && OptionValue[BReduce] && FCPatternFreeQ[{pp,mm1,mm2}]  && !MatchQ[{pp,mm1,mm2},{0,0,0}];

(* General case for $LimitTo4=True; The case with zero momentum and different masses is exluded *)
B11[pp_,mm1_,mm2_, OptionsPattern[]] :=
	PaVeReduce[PaVe[1,1,{pp},{mm1,mm2}],PaVeAutoReduce->True] /;
		!$LimitTo4 && OptionValue[BReduce] && FCPatternFreeQ[{pp,mm1,mm2}] && !(pp===0 && (mm1=!=mm2));

(* Special cases, valid for $LimitTo4=True; *)
b11[ 0,mm1_,mm1_ ] :=
	1/3 * B0[ 0,mm1,mm1 ];

b11[ SmallVariable[em_]^n_.,mm1_,mm1_ ] :=
	1/3 * B0[ SmallVariable[em]^n,mm1,mm1 ];

b11[pp:Except[_SmallVariable | 0], mm_, mm_] :=
	( 1/(3pp) ( A0[mm]+B0[pp,mm,mm] smad[pp-mm]- smad[mm - pp/6] ));

b11[pp:Except[_SmallVariable | 0], m1_, m2_] :=
	( 1/(3 pp) ( A0[m2] - smad[2 (pp-m2 + m1)]*(B1[pp,m1,m2]) - smad[m1] B0[pp,m1,m2] -
		smad[ 1/2 (m1 + m2 - pp/3 )]) );

B11 /:
	MakeBoxes[B11[p10_,m02_,m12_, OptionsPattern[]]  ,TraditionalForm] :=
		ToBoxes[HoldForm[PaVe[1,1,{p10},{m02,m12}]],TraditionalForm];

C0 /:
	MakeBoxes[C0[p10_,p12_,p20_,m02_,m12_,m22_, OptionsPattern[]]  ,TraditionalForm] :=
		ToBoxes[HoldForm[PaVe[0,{p10,p12,p20},{m02,m12,m22}]],TraditionalForm];

D0 /:
	MakeBoxes[D0[p10_,p12_,p23_,p30_,p20_,p13_,m02_,m12_,m22_,m32_, OptionsPattern[]]  ,TraditionalForm] :=
		ToBoxes[HoldForm[PaVe[0,{p10,p12,p23,p30,p20,p13},{m02,m12,m22,m32}]],TraditionalForm];

Derivative[1, 0, 0][B0][pp_,m02_,m12_] =
	DB0[pp,m02,m12];
(* also DB0 is symmetric in its mass arguments *)

DB0[pe_,me2_,me1_,opt:OptionsPattern[]] :=
	DB0 @@ Prepend[ {me1,me2,opt}, Expand[pe]] /; !OrderedQ[{me2,me1}];

Derivative[1, 0, 0][be1_][pp_,m02_,m12_] :=
	DB1[pp,m02,m12] /; be1 === B1;

DB1[m_, m_, 0, OptionsPattern[]] :=
	(- DB0[m,m,0] + 1/2/m) /; OptionValue[BReduce];

HoldPattern[DB1[pp:Except[_SmallVariable | 0], m02_, m12_, OptionsPattern[]]] :=
	(- (m12 - m02)/(2 pp^2) ( B0[pp,m02,m12] - B0[0,m02,m12] ) +
	(m12 - m02 - pp)/(2 pp) DB0[pp,m02,m12] ) /; OptionValue[BReduce];


FCPrint[1,"PaVeIntegrals.m loaded."];
End[]
