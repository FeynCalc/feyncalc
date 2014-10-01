(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PaVeIntegrals													*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Definitions of Passarino Veltman integrals and their
	derivatives															    *)

(* ------------------------------------------------------------------------ *)


BeginPackage["HighEnergyPhysics`fctables`PaVeIntegrals`",
             {"HighEnergyPhysics`FeynCalc`"}];

A0::"usage" =
"A0[m^2] is the Passarino-Veltman one point integral.";

A0ToB0::"usage" =
"A0ToB0 is an option for A0. If set to True, A0[m^2] is expressed
by (1+ B0[0,m^2,m^2])*m^2.";

B0::"usage" =
"B0[pp,m1^2,m2^2] is the Passarino-Veltman two point integral.
All arguments are scalars and have dimension mass^2.";

B0Real::"usage" =
"B0Real is an option of B0 (default False). If set to True,
B0 is assumed to be real and
the relation B0[a,0,a] = 2 + B0[0,a,a]  is applied.";

B0Unique::"usage" =
"B0Unique is an option of B0. If set to True, B0[0,0,m2] is replaced
with (B0[0,m2,m2]+1) and B0[m2,0,m2] simplifies to (B0[0,m2,m2]+2).";

B00::"usage" =
"B00[pp,m1^2,m2^2] is the Passarino-Veltman B00-function, i.e. the
coefficient function of g(mu nu). All arguments are scalars and have
dimension mass^2.";

B1::"usage" =
"B1[pp,m1^2,m2^2] is the Passarino-Veltman B1-function.
All arguments are scalars and have dimension mass^2.";

B11::"usage" =
"B11[pp,m1^2,m2^2] is the Passarino-Veltman B11-function, i.e.
the coefficient function of p(mu) p(nu).";

BReduce::"usage" =
"BReduce is an option for B0, B00, B1, B11 determining whether
reductions to A0 and B0 will be done. ";

C0::"usage" =
"C0[p10, p12, p20, m1^2, m2^2, m3^2] is the scalar
Passarino-Veltman C0-function.  The convention for the arguments
is that if the denominator of the integrand has the form
([q^2-m1^2] [(q+p1)^2-m2^2] [(q+p2)^2-m3^2]),
the first three arguments of C0 are the scalar products
 p10 = p1^2, p12 = (p1-p2).(p1-p2), p20 = p2^2.";

D0::"usage" =
"D0[ p10, p12, p23, p30, p20, p13,  m1^2, m2^2, m3^2, m4^2 ] is the
 Passarino-Veltman D0-function. The convention for the arguments is
that if the denominator of the integrand has the form
( [q^2-m1^2] [(q+p1)^2-m2^2] [(q+p2)^2-m3^2] [(q+p3)^2-m4^2] ),
 the first six arguments of D0 are the scalar products
p10 = p1^2, p12 = (p1-p2)^2, p23 = (p2-p3)^2, p30 = p3^2,
p20 = p2^2, p13 = (p1-p3)^2.";

DB0::"usage" =
"DB0[p2,m1^2,m2^2] is the derivative of the two-point function
B0[p2,m1^2,m2^2] with respect to p2.";

DB1::"usage" =
"DB1[p2,m1^2,m2^2] is the derivative of B1[p2,m1^2,m2^2] with respect to p2.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Factor2, FreeQ2, PaVe];
SmallVariable = MakeContext["CoreObjects","SmallVariable"];
PaVeReduce :=
    PaVeReduce = MakeContext["PaVeReduce"];

ClearAttributes[B0, ReadProtectecd];
ClearAttributes[B00, ReadProtectecd];
ClearAttributes[B1, ReadProtectecd];
ClearAttributes[B11, ReadProtectecd];

Options[A0] = {A0ToB0 -> False};
Options[B0] = {BReduce -> False, B0Unique -> False, B0Real -> False };
Options[B00] = {BReduce->True};
Options[B1] = {BReduce->True};
Options[B11] = {BReduce->True};
Options[DB1] = {BReduce->True};

bop[x___] :=
    BReduce/.Flatten[ Join[{x},Options[B0]] ];
nos[x_] :=
    If[ (x =!= 0) && FreeQ[x, SmallVariable],
        True,
        False
    ];
smanull[_] :=
    0;
smad[x_] :=
    Block[ {nx = Factor2[x]/.SmallVariable->smanull},
        Factor2[Numerator[nx]]/ Factor2[Denominator[nx]]
    ];
pcheck[zz__] :=
    FreeQ2[{zz},{Blank,BlankSequence,BlankNullSequence,Pattern}];


A0[SmallVariable[_]^_. , ___] :=
    0;  (* In dimensional regularization: A0(0)=0 *)
A0[0,___] = 0;
 (*A0[mm_, A0ToB0 -> False] := A0[mm];*)
A0[mm_, op___Rule] :=
    (mm + mm B0[0,mm,mm]) /;
    ( (( A0ToB0/.{op}/.Options[A0] )===True) && (!( BReduce/.Options[B0]))
    && FreeQ[mm,Pattern] && FreeQ[mm, BlankSequence] &&
    FreeQ[mm, Blank] && FreeQ[mm,BlankNullSequence] );

A0 /: MakeBoxes[A0[a_]  ,TraditionalForm] := Tbox[Subscript["A","0"], "(", a, ")"];

B0[pe_,me2_,me1_,opt___] :=
    B0 @@ Prepend[ {me1,me2,opt}, Expand[pe]] /; !OrderedQ[{me2,me1}];

B0[SmallVariable[pp_]^j_., SmallVariable[a_]^n_., SmallVariable[b_]^m_.] :=
    B0[pp^j, a^n, b^m];
B0[0, SmallVariable[a_]^n_., SmallVariable[b_]^m_.] :=
    B0[0, a^n, b^m];

B0[0,0,mm_,opt___] :=
    ( B0[0,mm,mm] + 1 ) /; nos[mm] &&
    ( (B0Unique/.{opt}/.Options[B0]) === True );

B0[mm_,0,mm_,opt___] :=
    ( B0[0,mm,mm] + 2 ) /;
    ( (B0Unique/.{opt}/.Options[B0]) === True ) &&
    ( (B0Real/.{opt}/.Options[B0]) === True );
(*
??? Changed Jan 97
 B0[SmallVariable[pp_]^n_., m1_, m2_, opt___] := B0[0, m1, m2, opt
                                           ]/;nos[m1]||nos[m2];
 B0[pp_, SmallVariable[m1_]^n_., m2_, opt___] := B0[pp, 0, m2, opt
                                           ]/;nos[pp]||nos[m2];
 B0[pp_, m1_, SmallVariable[m2_]^n_.,opt___ ] := B0[pp, m1, 0, opt
                                           ]/;nos[pp]||nos[m1];
*)
(* special cases *)
B0[kl_, kmm_, mm_, opt___ ] :=
    (A0[mm]/mm) /; nos[mm] && bop[opt] &&
            (((kl E+kmm)/.SmallVariable[_]->0)===0);

B0[kl_, mm_, kmm_, opt___ ] :=
    (A0[mm]/mm) /; nos[mm] && bop[opt] &&
          (((kl E+kmm)/.SmallVariable[_]->0)===0);

B0[0,mm_,mm_,opt___] :=
    (A0[mm]/mm - 1)/;nos[mm] && bop[opt];


(*fixed bop[opt] option Jan 1998*)
B0[0,m1_,m2_, opts___Rule] :=
    ((1/(m1-m2) A0[m1] - 1/(m1-m2) A0[m2]) /;
      m1 =!= m2) /; bop[opts];
(*
??? Changed Jan 97
 B0[SmallVariable[_]^n_.,mm_,mm_,opt___] :=
     (A0[mm]/mm - 1)/;nos[mm] && bop[opt];
*)
B0 /: MakeBoxes[B0[a_,b_,c_,___]  ,TraditionalForm] :=
    RowBox[{SubscriptBox["B","0"], "(",MakeBoxes[a,TraditionalForm],",",
        MakeBoxes[b,TraditionalForm], ",",MakeBoxes[c,TraditionalForm], ")"}];



B00[x__,  BReduce->True] :=
    b00[x] /; ($LimitTo4 === True) && pcheck[x];

B00[x__,  BReduce->True] :=
    PaVeReduce[PaVe[0,0,{First[{x}]},Rest[{x}]]] /;
    ($LimitTo4 === False) && pcheck[x];
B00[x_,y_,z_] :=
    b00[x,y,z]/;( BReduce/.Options[B00] )===True &&
                ($LimitTo4 === True) && pcheck[x,y,z];
B00[x_,y_,z_] :=
    B00[x,y,z, BReduce->True
       ]/;(BReduce/.Options[B00])===True &&
                ($LimitTo4 === False) && pcheck[x,y,z];

b00[0,mm_,mm_] :=
    mm / 2 ( B0[0,mm,mm] + 1 )/;nos[mm];
b00[SmallVariable[em_]^n_.,mm_,mm_] :=
    mm / 2 ( B0[em^n,mm,mm] + 1 )/;nos[mm];
b00[pp_,mm_,mm_] :=
    1/6 ( A0[mm]+B0[pp,mm,mm] smad[2 mm - pp/2] +
            smad[2 mm - pp/3]) /;nos[pp];
b00[pp_,mm1_,mm2_] :=
    ( 1/6 ( A0[mm2]+
    (B1[pp,mm1,mm2] ) smad[pp-mm2+mm1] )+
         smad[mm1/3] B0[pp,mm1,mm2] +
    smad[ 1/6 ( mm1 + mm2 - pp/3 ) ] );

B00 /: MakeBoxes[B00[a_,b_,c_,___Rule]  ,TraditionalForm] :=
  Tbox[Subscript["B","00"],"(",a,  ", ", b, ", ", c,")"]

B1[a_,b_,c_,ops___Rule] :=
    bb1[a, b, c] /;
    ((BReduce/.{ops}/.Options[B1])===True) &&
    (Head[bb1[a,b,c]] =!= bb1) &&
    FreeQ2[{a,b,c},
    {Blank, BlankSequence, BlankNullSequence, Pattern}];

(* Special cases, if photon and fermionic SmallVariable masses are present *)
bb1[SmallVariable[me_]^n_., SmallVariable[me_]^n_., SmallVariable[mla_]^m_.] :=
    ( -1/2 B0[SmallVariable[me]^n, SmallVariable[me]^n, 0] - 1/2 )/; TrueQ[mla < me];

bb1[SmallVariable[me_]^n_., SmallVariable[mla_]^n_., SmallVariable[me_]^m_.] :=
    (1/2 - 1/2 B0[SmallVariable[me]^n,0 ,SmallVariable[me]^n]) /; TrueQ[mla < me];

(* other special cases of B1 *)

(* B1( p,m,m ) = -1/2 B0( p,m,m )  *)
bb1[pp_,mm_,mm_] :=
    -1/2 B0[pp,mm,mm];
bb1[mm_, mm_, 0] :=
    -1/2 B0[mm, mm, 0] - 1/2;
bb1[mm_, 0, mm_] :=
    1/2 - B0[mm,0,mm]/2;
bb1[0,0,mm_] :=
    -1/2 B0[0,0,mm]+1/4;
bb1[SmallVariable[_]^n_.,0,mm_] :=
    ( -1/2 B0[0,0,mm] + 1/4 )/;nos[mm];
bb1[0,SmallVariable[_]^n_.,mm_] :=
    ( -1/2 B0[0,0,mm] + 1/4 )/;nos[mm];
bb1[0,mm_,0] :=
    ( -1/2 B0[0,0,mm] - 1/4 )/;nos[mm];

bb1[SmallVariable[_]^n_.,SmallVariable[_]^n_.,mm_] :=
    ( -1/2 B0[0,0,mm] + 1/4 )/;nos[mm];
bb1[SmallVariable[_]^n_.,mm_,SmallVariable[_]^n_.] :=
    ( -1/2 B0[0,0,mm] - 1/4 )/;nos[mm];
bb1[SmallVariable[_]^n_.,mm_,0] :=
    ( -1/2 B0[0,0,mm] - 1/4 )/;nos[mm];

(* B1 in general *)
bb1[pp_,ma0_,ma1_ ] :=
    (smad[ma1-ma0]/(2 pp) (B0[pp,ma0,ma1] -
    B0[0,ma0,ma1]) - 1/2 B0[pp,ma0,ma1]) /; nos[pp];

B1 /: MakeBoxes[B1[a_,b_,c_,___Rule]  ,TraditionalForm] :=
   RowBox[{SubscriptBox["B","1"], "(",
        MakeBoxes[a,TraditionalForm],",",
           MakeBoxes[b,TraditionalForm],",",
             MakeBoxes[c,TraditionalForm],")"}
         ];

B11[pe_, mm1_, mm2_,  BReduce->True] :=
    b11[pe, mm1, mm2] /; ($LimitTo4 === True) && pcheck[pe,mm1,mm2] &&
                         (nos[pe] || ( (!nos[pe]) && (mm1 === mm2)));

B11[x__,  BReduce->True] :=
    PaVeReduce[PaVe[1,1,{{x}[[1]]}, Rest[{x}] ]] /;
       ($LimitTo4 === False) && (x[[1]] =!=0) && pcheck[x];

B11[x_,y_,z_] :=
    b11[x,y,z]/; (( BReduce/.Options[B11] )===True )&&
                 ($LimitTo4 === True ) && pcheck[x,y,z] &&
                 (nos[x] || ( (!nos[x]) && (y === z)));

B11[x_,y_,z_] :=
    B11[x,y,z, BReduce->True]/;( BReduce/.Options[B11]
                                )===True && pcheck[x,y,z] &&
                 ($LimitTo4 === False) && nos[x];
b11[ 0,mm1_,mm1_ ] :=
    1/3 * B0[ 0,mm1,mm1 ];
(*??
 b11[ SmallVariable[_]^n_.,mm1_,mm1_ ] := 1/3 * B0[ 0,mm1,mm1 ];
*)
b11[ SmallVariable[em_]^n_.,mm1_,mm1_ ] :=
    1/3 * B0[ SmallVariable[em]^n,mm1,mm1 ];
b11[ pp_,mm_,mm_] :=
    ( 1/(3pp) ( A0[mm]+B0[pp,mm,mm] smad[pp-mm]-
                         smad[mm - pp/6] )) /;nos[pp];
b11[ pp_,m1_,m2_ ] :=
    ( 1/(3 pp) ( A0[m2] - smad[2 (pp-m2 + m1)]*
     (PaVe[1,{pp},{m1,m2}]) - smad[m1] B0[pp,m1,m2] -
      smad[ 1/2 (m1 + m2 - pp/3 )]) )/;nos[pp];


B11 /: MakeBoxes[B11[a_,b_,c_,___Rule]  ,TraditionalForm] :=
  Tbox[Subscript["B","11"],"(",a,  ", ", b, ", ", c,")"];


C0 /: MakeBoxes[C0[a_,b_,c_,d_,e_,f_, ___Rule] ,TraditionalForm] :=
   Tbox[Subscript["C", "0"], "(", a, ",", b, ",",
        c, ",", d, ",", e, ",",f,")"];


D0 /: MakeBoxes[D0[a_,b_,c_,d_,e_,f_,h_,i_,j_,k_, ___Rule]  ,TraditionalForm] :=
   Tbox[Subscript["D","0"], "(",a,",",b,",",c,",",d,",",e,",",f,
                                  ",",h,",",i,",",j,",",k,")"];

Derivative[1, 0, 0][B0][pp_,m02_,m12_] = DB0[pp,m02,m12];
(* also DB0 is symmetric in its mass arguments *)
DB0[pe_,me2_,me1_,opt___] :=
    DB0 @@ Prepend[ {me1,me2,opt}, Expand[pe]] /; !OrderedQ[{me2,me1}];

Derivative[1, 0, 0][be1_][pp_,m02_,m12_] :=
    DB1[pp,m02,m12] /; be1 === B1;

DB1[m_, m_, 0, opt___] :=
    (- DB0[m,m,0] + 1/2/m) /; (BReduce/.{opt}/.
    Options[DB1]) === True

HoldPattern[DB1[pp_, m02_, m12_, opt___]] :=
    (
    - (m12 - m02)/(2 pp^2) ( B0[pp,m02,m12] - B0[0,m02,m12] ) +
    (m12 - m02 - pp)/(2 pp) DB0[pp,m02,m12] ) /; nos[pp] && ( (BReduce/.{opt}/.
    Options[DB1]) === True );


End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0,
    WriteString["stdout", "PaVeIntegrals | \n "]
];
Null
