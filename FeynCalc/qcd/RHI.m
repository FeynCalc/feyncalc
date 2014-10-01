(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: RHI *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 4 December '98 at 14:04 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  eq (3C.19) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`RHI`",{"HighEnergyPhysics`FeynCalc`"}];

RHI::"usage"= "RHI[{v,w,x,y,z},{a,b,c,d,e,f,g}, {al,be,ga,de,ep}]. (sn -> 1,
mark1 -> 1, mark2 -> 1, mark3 -> 1, eph -> Epsilon/2 ).
The exponents of the numerator scalar product are (dl = OPEDelta): \n\n

v: k1.k1, w: k2.k2,  x: p.k1, y: p.k2, z: k1.k2. \n\n

a: dl.k1, b: dl.k2,  c: dl.(p-k1), d: dl.(p-k2), e: dl.(k1-k2),
f: dl.(p+k1-k2), g: dl.(p-k1-k2) \n\n

RHI[any___,{a,b,c,d,e,0,0}, {al,be,ga,de,ep}] simplifies to
RHI[any, {a,b,c,d,e}, {al,be,ga,de,ep}]; \n\n

RHI[{0,0,0,0,0},{a,b,c,d,e}, {al,be,ga,de,ep}] simplifies to
RHI[{a,b,c,d,e}, {al,be,ga,de,ep}].";

FORM::"usage" =
"FORM is a bolean option telling FeynCalc whether or not to use FORM for
evaluation. If set to True a FORM file is generated and run from Mathematica
and the result read back in. Currently, only RHI has this option and it is
required to be on a UNIX system and have R. Hamberg's FORM-program installed
correctly.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(*nosaveDir = MakeContext["nosaveDir"];*)
Collect2 = MakeContext["Collect2"];
DeltaFunction = MakeContext["CoreObjects","DeltaFunction"];
Epsilon  = MakeContext["CoreObjects","Epsilon"];
EpsilonOrder = MakeContext["CoreOptions","EpsilonOrder"];
FAD        = MakeContext["CoreObjects","FAD"];
Factor1  = MakeContext["Factor1"];
Factoring  = MakeContext["CoreOptions","Factoring"];
FreeQ2   = MakeContext["FreeQ2"];
Momentum = MakeContext["CoreObjects","Momentum"];
OPEDelta = MakeContext["OPEDelta"];
OPEm     = MakeContext["OPEm"];
Pair     = MakeContext["CoreObjects","Pair"];
PowerSimplify = MakeContext["PowerSimplify"];
SOD      = MakeContext["CoreObjects","SOD"];
SPD      = MakeContext["CoreObjects","SPD"];
ScalarProduct = MakeContext["ScalarProduct"];
Select1  = MakeContext["Select1"];
Select2  = MakeContext["Select2"];
TLI = MakeContext["TLI"];

Options[RHI] =  {Directory -> "rh/ope/diagrams/",
                 (*nosaveDir -> False["rhisave"],*)
                 EpsilonOrder -> 0,
                 FORM -> False,
                 Momentum -> Global`p};



RHI[{v_,w_,x_,y_,z_},{a_,b_,c_,d_,e_},
    {al_,be_,ga_,de_,ep_}, {k1_, k2_},o___Rule ] := Block[
    {p = Momentum /. {o} /. Options[RHI]},
 ( (k1.k1)^v (k2.k2)^w (p.k1)^x (p.k2)^y (k1.k2)^z *
   SOD[k1]^a SOD[k2]^b SOD[p-k1]^c SOD[p-k2]^d SOD[k1-k2]^e /
   (SPD[k1,k1]^al SPD[k2,k2]^be SPD[k1-p,k1-p]^ga SPD[k2-p,k2-p]^de *
    SPD[k1-k2,k1-k2]^ep)) /. Dot->SPD];



(* new *)
RHI[{0,0,x_,j_,0}, {0, b_Integer?Positive, c_Integer?Positive, d_,
                           e_Integer?Positive}, opt___] :=
 RHI[{j,x,0,0,0},{d,c,b,0,e}, opt] /;
    (j=!=0) && ((d === 0) || (d === -1)) ;


RHI[{a_, b_, c_, d_, e_}, {1, 1, 2, -1, 1}, opt___] :=
Block[{p, sop, spp},
  p = Momentum /. {opt} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
   spp = Pair[Momentum[p,D], Momentum[p,D]];
    2*sop*RHI[{-2 + a, 1 + b, c, d, e}, {0, 1, 2, 0, 1}] -
     sop*RHI[{-1 + a, b, c, d, e}, {0, 1, 2, 0, 1}] -
     RHI[{-1 + a, 1 + b, c, d, e}, {0, 1, 2, 0, 1}] +
     RHI[{-1 + a, 1 + b, c, d, e}, {1, 1, 1, 0, 1}] -
     spp*RHI[{-1 + a, 1 + b, c, d, e}, {1, 1, 2, 0, 1}] +
     spp*RHI[{a, b, c, d, e}, {1, 1, 2, 0, 1}]
     ];

   RHI[{a_, b_, c_, d_, e_}, {1, 2, 1, -1, 1}, opt___] :=
Block[{p, sop, spp},
  p = Momentum /. {opt} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
   spp = Pair[Momentum[p,D], Momentum[p,D]];
    2*sop*RHI[{-2 + a, 1 + b, c, d, e}, {0, 2, 1, 0, 1}] -
     sop*RHI[{-1 + a, b, c, d, e}, {0, 2, 1, 0, 1}] -
     sop*RHI[{-1 + a, b, c, d, e}, {1, 1, 1, 0, 1}] -
     RHI[{-1 + a, 1 + b, c, d, e}, {0, 2, 1, 0, 1}] -
     spp*RHI[{-1 + a, 1 + b, c, d, e}, {1, 2, 1, 0, 1}] +
     RHI[{a, b, c, d, e}, {1, 1, 1, 0, 1}] +
     spp*RHI[{a, b, c, d, e}, {1, 2, 1, 0, 1}]
     ];

   RHI[{a_, b_, c_, d_, e_}, {2, 1, 1, -1, 1}, opt___] :=
Block[{p, sop, spp},
  p = Momentum /. {opt} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
   spp = Pair[Momentum[p,D], Momentum[p,D]];
    2*sop*RHI[{-2 + a, 1 + b, c, d, e}, {1, 1, 1, 0, 1}] -
     sop*RHI[{-1 + a, b, c, d, e}, {1, 1, 1, 0, 1}] -
     RHI[{-1 + a, 1 + b, c, d, e}, {1, 1, 1, 0, 1}] -
     spp*RHI[{-1 + a, 1 + b, c, d, e}, {2, 1, 1, 0, 1}] +
     spp*RHI[{a, b, c, d, e}, {2, 1, 1, 0, 1}]
     ];

   RHI[{a_, b_, c_, d_, e_}, {2, 1, 2, -1, 1}, opt___] :=
Block[{p, sop, spp},
  p = Momentum /. {opt} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
   spp = Pair[Momentum[p,D], Momentum[p,D]];
    2*sop*RHI[{-2 + a, 1 + b, c, d, e}, {1, 1, 2, 0, 1}] -
     sop*RHI[{-1 + a, b, c, d, e}, {1, 1, 2, 0, 1}] -
     RHI[{-1 + a, 1 + b, c, d, e}, {1, 1, 2, 0, 1}] +
     RHI[{-1 + a, 1 + b, c, d, e}, {2, 1, 1, 0, 1}] -
     spp*RHI[{-1 + a, 1 + b, c, d, e}, {2, 1, 2, 0, 1}] +
     spp*RHI[{a, b, c, d, e}, {2, 1, 2, 0, 1}]
     ];

RHI[{a_, b_, c_, d_, e_}, {1, 2, -1, 1, 1}, opt___Rule] :=
Block[{p,sop,spp},
  p = Momentum /. {opt} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
   spp = Pair[Momentum[p,D], Momentum[p,D]];
   -((-1)^e*RHI[{-1 + b, 1 + a, d, c, e}, {1, 1, 1, 0, 1},opt]) +
    2*(-1)^e*RHI[{-2 + b, 1 + a, d, c, e}, {1, 1, 1, 0, 1},opt]*sop -
    (-1)^e*RHI[{-1 + b, a, d, c, e}, {1, 1, 1, 0, 1},opt]*sop -
    (-1)^e*RHI[{-1 + b, 1 + a, d, c, e}, {2, 1, 1, 0, 1},opt]*spp +
    (-1)^e*RHI[{b, a, d, c, e}, {2, 1, 1, 0, 1},opt]*spp
     ];

(*NOV*)
RHI[{0,0,0,0,1},{aa__},{0,be_/;be>0,ga_,0,ep_/;ep>0},o___Rule]:=
Expand[
 -1/2 RHI[{aa},{0,be,ga,0,ep-1},o]+
  1/2 RHI[{aa},{-1,be,ga,0,ep},o] +
  1/2 RHI[{aa},{0,be-1,ga,0,ep},o] ];
(*PROJNOV*)
RHI[{0, 0, 0, 1, 0}, {a_, b_, c_, d_, e_},
    {al_, (be_Integer)?Positive, ga_, 0, (ep_Integer)?Positive},o___Rule
   ] := (Block[{sop,p},
   p = Momentum /. {o} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
  (rRHI[{0, 0, 1, 0, 0}, {-1 + a, 1 + b, c, d, e}, {al, be, ga, 0, ep},o] -
    rRHI[{-2 + a, 1 + b, c, d, e}, {-1 + al, be, ga, 0, ep},o]*sop +
    rRHI[{0, 0, 0, 0, 1}, {-1 + a, b, c, d, e}, {al, be, ga, 0, ep},o]*sop
  )//Expand]/.rRHI->RHI) /; ((b =!= 0) || (d =!= 0) || (e =!= 0));

RHI[{0, 0, 1, 0, 0}, {a_, b_, c_, d_, e_},
    {(al_Integer)?Positive, be_, 0, de_, (ep_Integer)?Positive},
    opt___Rule
   ] := Block[{sop,p},
   p = Momentum /. {opt} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
  (RHI[{0, 0, 0, 1, 0}, {1 + a, -1 + b, c, d, e}, {al, be, 0, de, ep}] -
    (-1)^e*RHI[{-2 + b, 1 + a, d, c, e}, {-1 + be, al, de, 0, ep}]*sop +
    RHI[{0, 0, 0, 0, 1}, {a, -1 + b, c, d, e}, {al, be, 0, de, ep}]*sop
  )//Expand] /; ((a =!= 0) || (c =!= 0) || (e =!= 0));

RHI[{0, 0, 0, 2, 0}, {a_, b_, c_, d_, e_},
    {al_, (be_Integer)?Positive, ga_, 0, (ep_Integer)?Positive},
    opt___Rule
   ] := Block[{sop,p},
   p = Momentum /. {opt} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
(RHI[{0, 0, 2, 1, 0}, {-2 + a, 2 + b, c, d, e}, {al, be, ga, 0, ep}] -
    2*RHI[{0, 0, 1, 1, 0}, {-3 + a, 2 + b, c, d, e},
      {-1 + al, be, ga, 0, ep}]*sop +
    (2*RHI[{0, 0, 1, 1, 0}, {-3 + a, 2 + b, c, d, e},
        {-1 + al, be, ga, 0, ep}]*sop)/(2 - D) +
    (2*RHI[{0, 0, 1, 1, 0}, {-1 + a, b, c, d, e},
        {al, -1 + be, ga, 0, ep}]*sop)/(2 - D) +
    2*RHI[{0, 0, 1, 1, 1}, {-2 + a, 1 + b, c, d, e}, {al, be, ga, 0, ep}]*
     sop - (4*RHI[{0, 0, 1, 1, 1}, {-2 + a, 1 + b, c, d, e},
        {al, be, ga, 0, ep}]*sop)/(2 - D) +
    RHI[{0, 0, 0, 1, 0}, {-4 + a, 2 + b, c, d, e},
      {-2 + al, be, ga, 0, ep}]*sop^2 -
    (RHI[{0, 0, 0, 1, 0}, {-4 + a, 2 + b, c, d, e},
        {-2 + al, be, ga, 0, ep}]*sop^2)/(2 - D) -
    (RHI[{0, 0, 0, 1, 0}, {-2 + a, b, c, d, e},
        {-1 + al, -1 + be, ga, 0, ep}]*sop^2)/(2 - D) -
    2*RHI[{0, 0, 0, 1, 1}, {-3 + a, 1 + b, c, d, e},
      {-1 + al, be, ga, 0, ep}]*sop^2 +
    (2*RHI[{0, 0, 0, 1, 1}, {-3 + a, 1 + b, c, d, e},
        {-1 + al, be, ga, 0, ep}]*sop^2)/(2 - D) +
    RHI[{0, 0, 0, 1, 2}, {-2 + a, b, c, d, e}, {al, be, ga, 0, ep}]*
     sop^2 - (RHI[{0, 0, 0, 1, 0}, {-2 + a, 2 + b, c, d, e},
        {-1 + al, be, ga, 0, ep}]*SPD[p, p])/(2 - D) -
    (RHI[{0, 0, 0, 1, 0}, {a, b, c, d, e}, {al, -1 + be, ga, 0, ep}]*
       SPD[p, p])/(2 - D) + (2*
       RHI[{0, 0, 0, 1, 1}, {-1 + a, 1 + b, c, d, e}, {al, be, ga, 0, ep}]*
       SPD[p, p])/(2 - D)
 )//Expand] /; ((b =!= 0) || (d =!= 0) || (e =!= 0));

RHI[{0, 0, 2, 0, 0}, {a_, b_, c_, d_, e_},
    {(al_Integer)?Positive, be_, 0, de_, (ep_Integer)?Positive},
    opt___Rule
   ] := Block[{sop,p},
   p = Momentum /. {opt} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
(RHI[{0, 0, 0, 2, 0}, {2 + a, -2 + b, c, d, e}, {al, be, 0, de, ep}] +
    (2*RHI[{0, 0, 0, 1, 0}, {a, -1 + b, c, d, e},
        {-1 + al, be, 0, de, ep}]*SOD[p])/(2 - D) -
    2*RHI[{0, 0, 0, 1, 0}, {2 + a, -3 + b, c, d, e},
      {al, -1 + be, 0, de, ep}]*SOD[p] +
    (2*RHI[{0, 0, 0, 1, 0}, {2 + a, -3 + b, c, d, e},
        {al, -1 + be, 0, de, ep}]*SOD[p])/(2 - D) +
    2*RHI[{0, 0, 0, 1, 1}, {1 + a, -2 + b, c, d, e}, {al, be, 0, de, ep}]*
     SOD[p] - (4*RHI[{0, 0, 0, 1, 1}, {1 + a, -2 + b, c, d, e},
        {al, be, 0, de, ep}]*SOD[p])/(2 - D) +
    (-1)^e*RHI[{-4 + b, 2 + a, d, c, e}, {-2 + be, al, de, 0, ep}]*
     SOD[p]^2 - ((-1)^e*RHI[{-4 + b, 2 + a, d, c, e},
        {-2 + be, al, de, 0, ep}]*SOD[p]^2)/(2 - D) -
    ((-1)^e*RHI[{-2 + b, a, d, c, e}, {-1 + be, -1 + al, de, 0, ep}]*
       SOD[p]^2)/(2 - D) - 2*RHI[{0, 0, 0, 0, 1},
      {1 + a, -3 + b, c, d, e}, {al, -1 + be, 0, de, ep}]*SOD[p]^2 +
    (2*RHI[{0, 0, 0, 0, 1}, {1 + a, -3 + b, c, d, e},
        {al, -1 + be, 0, de, ep}]*SOD[p]^2)/(2 - D) +
    RHI[{0, 0, 0, 0, 2}, {a, -2 + b, c, d, e}, {al, be, 0, de, ep}]*
     SOD[p]^2 - ((-1)^e*RHI[{-2 + b, 2 + a, d, c, e},
        {-1 + be, al, de, 0, ep}]*SPD[p, p])/(2 - D) -
    ((-1)^e*RHI[{b, a, d, c, e}, {be, -1 + al, de, 0, ep}]*SPD[p, p])/
     (2 - D) + (2*RHI[{0, 0, 0, 0, 1}, {1 + a, -1 + b, c, d, e},
        {al, be, 0, de, ep}]*SPD[p, p])/(2 - D)
  )//Expand] /; ((a =!= 0) || (c =!= 0) || (e =!= 0));


(*
HAEH??
RHI[{0, 0, 0, 0, 1}, {a_, b_, c_, d_, e_}, {1, 1, 2, 1, 0}, opt___Rule] :=
 Block[{p, sop, spd},
   p = Momentum /. {opt} /. Options[RHI];
   sop = Pair[Momentum[OPEDelta,D], Momentum[p,D]];
   spp = Pair[Momentum[p,D], Momentum[p,D]];
   -RHI[{a, 1 + b, c, d, e}, {1, 1, 1, 1, 0}, opt]/(2*sop) -
    (RHI[{1 + a, 1 + b, c, d, e}, {1, 1, 2, 1, 0}, opt]*spp)/sop^2 +
    (RHI[{a, 1 + b, c, d, e}, {1, 1, 2, 1, 0}, opt]*spp)/(2*sop) +
    (RHI[{1 + a, b, c, d, e}, {1, 1, 2, 1, 0}, opt]*spp)/(2*sop)
      ];
*)

(*NOV96*)
RHI[{a_Integer?Positive,b__},{c__},{al_,be__},opt___Rule] :=
 RHI[{0,b},{c},{al-a,be},opt] /; al>=a;
RHI[{w_,a_Integer?Positive,b__},{c__},{xi_,al_,be__},opt___Rule] :=
 RHI[{w,0,b},{c},{xi,al-a,be},opt] /; al>=a;

(*
(*1<-->2*)
RHI[{a_,b_,c_,d_,e_},{al_/;al<=0,be_/;be>=0,ga_,de_, ep_},opt___Rule] :=
(-1)^e RHI[{b,a,d,c,e},{be,al,de,ga,ep},opt];
*)

RHI[{v_,w_,x_,y_,z_},
    {a_,b_,c_,d_,e_},{al_/;al<=0,be_,ga_,de_Integer?Positive,
                      ep_Integer?Positive},opt___Rule] :=
(-1)^e RHI[{w,v,y,x,z},{b,a,d,c,e},{be,al,de,ga,ep},opt];

(* k2->-k2+p; k1->-k1+p *)
RHI[{a_,b_,c_,d_,e_},{al_,be_/;be<=0,ga_,de_Integer?Positive,
                      ep_Integer?Positive},opt___Rule] :=
(-1)^e RHI[{c,d,a,b,e},{ga,de,al,be,ep},opt];

(*p.k1*)
RHI[{a_,b_,c_Integer?Positive,d__},{vv__},
    {al_Integer?Positive,be_,ga_Integer?Positive, de__},opt___Rule] :=
Block[{p = Momentum /. {opt} /. Options[RHI]},
(*  1/2 p^2 *)
1/2 Pair[Momentum[p,D],Momentum[p,D]]*
   RHI[{a,b,c-1,d},{vv},{al,be,ga,de},opt]+
(* 1/2 k1^2 *)
1/2 RHI[{a,b,c-1,d},{vv},{al-1,be,ga,de},opt] -
1/2 RHI[{a,b,c-1,d},{vv},{al,be,ga-1,de},opt]
     ]//Expand;

(*p.k2*)
RHI[{a_,b_,c_,d_Integer?Positive,e_},{vv__},
    {al_,be_Integer?Positive,ga_,de_Integer?Positive, ep_},opt___Rule] :=
Block[{p = Momentum /. {opt} /. Options[RHI]},
(*  1/2 p^2 *)
1/2 Pair[Momentum[p,D],Momentum[p,D]] RHI[{a,b,c,d-1,e},{vv},
                                      {al,be,ga,de,ep},opt]+
(* 1/2 k2^2 *)
1/2 RHI[{a,b,c,d-1,e},{vv},{al,be-1,ga,de,ep},opt] -
1/2 RHI[{a,b,c,d-1,e},{vv},{al,be,ga,de-1,ep},opt]
     ]//Expand;

(*k1.k2*)
RHI[{a_,b_,c_,d_,e_Integer?Positive},{vv__},
    {al_Integer?Positive,be_Integer?Positive,ga_Integer?Positive,de_, ep_},
      opt___Rule] :=
Expand[
(*  1/2 k1^2 *)
1/2 RHI[{a,b,c,d,e-1},{vv}, {al-1,be,ga,de,ep},opt]+
(* 1/2 k2^2 *)
1/2 RHI[{a,b,c,d,e-1},{vv},{al,be-1,ga,de,ep},opt] -
(*- 1/2 (k1-k2)^2 *)
1/2 RHI[{a,b,c,d,e-1},{vv},{al,be,ga-1,de,ep},opt]
      ] ;

(*
RHI[{
*)


RHI[{a_,b_,c_,d_,e_},{al_,0,ga_,de_ /; de =!= 0,ep_}, opt___Rule] :=
 (-1)^e RHI[{c,d,a,b,e}, {ga,de,al,0,ep}];

(* q1 -> -q1+p; q2 -> -q2+p *)
RHI[{1,0,0,0,0},{a_,b_,c_,d_,e_},{0,be_,ga_/;ga>0,de_,ep_},o___Rule] :=
 Expand[
 PowerSimplify[(-1)^e] RHI[{c,d,a,b,e},{ga-1,de,0,be,ep}] -
  2 PowerSimplify[(-1)^e] RHI[{0,0,1,0,0},{c,d,a,b,e},{ga,de,0,be,ep}]+
   (Pair[Momentum[#,D], Momentum[#,D]]&[Momentum/.{o}/.Options[RHI]]) *
    RHI[{c,d,a,b,e},{ga,de,0,be,ep}] ];

(*
RHI[{1,0,0,0,0}, {a__},{0,b__}, opt___Rule] := RHI[{a},{-1,b},opt];
RHI[{2,0,0,0,0}, {a__},{0,b__}, opt___Rule] := RHI[{a},{-2,b},opt];
*)

(* new  05/96 *)
RHI[{0,0,0,0,1}, {a__}, {al_,0, ga_,de_,ep_}, opt___Rule] :=
-1/2 RHI[{a}, {al,0,ga,de,ep-1}, opt] +
 1/2 RHI[{a}, {al-1,0,ga,de,ep}, opt] +
 1/2 RHI[{a}, {al,-1,ga,de,ep }, opt];

RHI[{-1,b_Integer?Positive,c_Integer,d_Integer,e_/;Head[e]=!=Integer},
    {dd__}, opt___Rule
   ] := - RHI[{-1, b-1,c,d,e+1}, {dd}, opt] +
          RHI[{0,b-1,c,d,e}, {dd}, opt];

RHI[{a_Integer?Positive,-1,c_Integer,d_Integer,e_/;Head[e]=!=Integer},
    {dd__}, opt___Rule
   ] :=   RHI[{a-1,-1,c,d,e+1}, {dd}, opt] +
          RHI[{a-1,0,c,d,e},    {dd}, opt];

RHI[{0,-1,0,0,e_/;Head[e]=!=Integer},
    {1,1,1,1,1}, opt___Rule
   ] := PowerSimplify[(-1)^e] RHI[{-1,0,0,0,e},{1,1,1,1,1},opt];

RHI[{a_,b_,c_,d_,e_Integer?Positive},{bla__}, opt___Rule] :=
Sum[Binomial[e,ii] (-1)^(e-ii) RHI[{a+ii, b+e-ii,c,d,0}, {bla}, opt
                                  ], {ii,0,e}]//Expand;

RHI[{a_,n_Integer?Positive,c_,d_,-1},{bla__}, opt___Rule] :=
-RHI[{a,n-1,c,d,0},{bla},opt] + RHI[{a+1,n-1,c,d,-1},{bla},opt];

(*
can give recursion with above
RHI[{n_Integer?Positive,b_,c_,d_,-1},{bla__}, opt___Rule] :=
 RHI[{n-1,b,c,d,0},{bla},opt] + RHI[{n-1,b+1,c,d,-1},{bla},opt];
*)

(* k1 <--> k2 *)
(* f441*)
RHI[{a_,b_,c_,d_,e_},{al_,be_,0,de_ /; de=!=0, ep_},opt___Rule] :=
(-1)^e RHI[{b, a, d, c, e}, {be, al, de, 0, ep}, opt];

RHI[{a_,b_,c_,d_,e_},{al_,be_,-1,de_ /; de=!=0, ep_},opt___Rule] :=
(-1)^e RHI[{b, a, d, c, e}, {be, al, de, -1, ep}, opt];

(* f442*)
RHI[{a_,b_Integer,c_,0,e_ /; Head[e]=!=Integer},
    {al_,be_,ga_,0,  ep_},opt___Rule
   ] := RHI[{a,e,c,0,b},{al,ep,ga,0,be}];


RHI[{0,0,0,0,0},{x__},{y__}, opt___Rule] := RHI[{x}, {y}, opt];
RHI[{x1_,x2_,x3_,x4_,x5_, 0, 0}, {y__}] :=  RHI[{x1,x2,x3,x4,x5},{y}];
RHI[any_List, {x1_,x2_,x3_,x4_,x5_, 0, 0}, {y__}] :=
 RHI[any, {x1,x2,x3,x4,x5}, {y}];

RHI[x1_,x2_,x3_,x4_,x5_, x6_,x7_,x8_, x9_,x10_, x10b_, x10c_,
    x11_,x12_,x13_,x14_,x15_, opt___Rule]:=
RHI[{x1,x2,x3,x4,x5}, {x6, x7, x8, x9, x10, x10b, x10c},
    {x11,x12,x13,x14,x15}, opt];
RHI[x1_,x2_,x3_,x4_,x5_, x6_,x7_,x8_, x9_,x10_, x11_,x12_,x13_,x14_,x15_,
    opt___Rule]:=
  RHI[{x1,x2,x3,x4,x5}, {x6, x7, x8,  x9, x10}, {x11,x12,x13,x14,x15},opt];

RHI[x1_,x2_,x3_,x4_,x5_, x6_,x7_,x8_, x9_,x10_, opt___Rule] :=
RHI[{x1,x2,x3,x4,x5},   {x6, x7, x8,  x9, x10}, opt];

RHI[x1_,x2_,x3_,x4_,x5_, x5b_, x5c_, x6_,x7_,x8_, x9_,x10_, opt___Rule]:=
RHI[{x1,x2,x3,x4,x5,x5b, x5c},   {x6, x7, x8,  x9, x10}, opt];

(* eq. (3C.20) *)
RHI[___,{__},{_,_,_,0,0}] = 0;
RHI[___,{__},{_,_,0,_,0}] = 0;
RHI[___,{__},{_,_,0,0,_}] = 0;
RHI[___,{__},{_,0,_,_,0}] = 0;
RHI[___,{__},{_,0,_,0,_}] = 0;
RHI[___,{__},{0,_,_,_,0}] = 0;
RHI[___,{__},{0,_,0,_,_}] = 0;
RHI[___,{__},{0,0,_,_,_}] = 0;

(* finite *)
(* careful: the first argument is only ONE list *)
 RHI[ll_List, {a_ /; a>0, b_ /; b>0, c_ /; c>0, d_ /; d>0, e_ /;e>0},
     opt___Rule
    ] := 0 /; (EpsilonOrder /. {opt} /. Options[RHI]) < 0 &&
            (Apply[Plus, Select1[ll,OPEm]]>=0);

RHI[snum___List, {a_,b_,c_,d_,e_,f_,g_},{al_,be_,ga_,de_,ep_}, opt___Rule] :=
loadrhi[snum,{a,b,c,d,e,f,g},{al,be,ga,de,ep}, nosaveDir /. {opt} /.
        Options[RHI], opt
       ] /;(loadrhi[snum,{a,b,c,d,e,f,g},{al,be,ga,de,ep},
                    nosaveDir /. {opt} /.  Options[RHI], opt
                   ] =!= False
           ) && StringQ[nosaveDir /. {opt} /.Options[RHI]];

RHI[snum___List, {a_,b_,c_,d_,e_},{al_,be_,ga_,de_,ep_}, opt___Rule] :=
loadrhi[snum,{a,b,c,d,e},{al,be,ga,de,ep}, nosaveDir /. {opt} /.
        Options[RHI], opt
       ] /;(loadrhi[snum,{a,b,c,d,e},{al,be,ga,de,ep},
                    nosaveDir /. {opt} /.  Options[RHI], opt
                   ] =!= False
           ) && StringQ[nosaveDir /. {opt} /. Options[RHI]];

loadrhi[_List,_List,lastdir_ /; (Head[lastdir] =!= String) &&
                       (Head[lastdir] =!= Rule), ___Rule ] := False;
loadrhi[_List,_List,_List,lastdir_ /; (Head[lastdir] =!= String) &&
                       (Head[lastdir] =!= Rule), ___Rule ] := False;

Off[FileNames::cdir];
(* constructing a unique file name *)
st[x__]  := StringReplace[StringJoin @@ Map[ToString, {x}],
                          {"-1" -> "-"}];
stD[x__] := StringReplace["D" <> (StringJoin @@ Map[ToString, {x}]),
                          {"-1" -> "-"}];
(* shortdef *)
short[x_] := StringReplace[ToString[x /.
                  { Times :> est, Plus :> est,
                    Pair  :> est, Power :> est
                  } /.  {est :> st}], {"[" -> "", "]" -> "", ", " -> ""}
                          ];
(*XXX*)
frh = FixedPoint[ReleaseHold, #]&;
loadrhi[indi___List, save_String, opt___Rule] :=
loadrhi[indi,save,opt] =
 Block[{ind,file, filesave, checkifthere, re = False},
(*
        ind = Flatten[{indi}];
*)
ind = {indi};
        file         = short[frh["RHI"@@Flatten[ind]
                                ]
                            ] <> ".i";

  (*Mac fix, 18/9-2000, F.Orellana*)
  checkifthere = FileNames[{file}, {$FeynCalcDirectory <>$PathnameSeparator<>save}];
  FCPrint[1,"checkifthere = ", checkifthere];
     If[checkifthere =!= {},
        FCPrint[1,"Loading"];
        re = Get @@ checkifthere;
        FCPrint[1, " re = ",re];
        If[re === Null, checkifthere = {}];
       ];
     If[checkifthere === {},
        If[(FORM /. {opt} /. Options[RHI]) === True,
           re = RHI@@Join[ind, {FORM -> True,
                    (* to avoid an infinite loop *)
                    nosaveDir -> False}
                   ];
           If[re =!= Null,
           (*filesave     = $FeynCalcDirectory <> "/" <> save <> "/" <> file;*)
           (*We should try to be cross-platform. F.Orellana, 8/9-2002.*)
           filesave     = ToFileName[{$FeynCalcDirectory, save}, file];
FCPrint[1,"writing file ",filesave];
           OpenWrite @@ {filesave, FormatType -> InputForm };
           WriteString @@ {filesave,
                           ToString[frh["RHI"@@Flatten[ind]]
                                   ] <> " = \n ( "};
           Write       @@ {filesave, re};
           WriteString @@ {filesave, ") "};
           Close       @@ {filesave};
             ];
          ];
       ];  re];

scn[]=1;
scn[{a_,b_,c_,d_,e_}]:= "k1.k1"^a "k2.k2"^b "k1.p"^c "k2.p"^d "k1.k2"^e;

epcut[x_, op___Rule] := If[(EpsilonOrder /. {op} /. Options[RHI]
                           ) === -2,
                           Select2[x, Epsilon^(-2)],
                           x];

RHI[snum___List,{a_,b_,c_,d_,e_},{al_,be_,ga_,de_,ep_}, opt___Rule] :=
(
RHI[snum,{a,b,c,d,e}, {al,be,ga,de,ep}(*, opt*)] =
epcut[
fromform[
"l M = " <> StringReplace[
ToString[InputForm[
scn[snum]*
pw[Global`d1, a] pw[Global`d2, b] pw[Global`d3, c] *
pw[Global`d4, d] pw[Global`d5, e] /
(Global`N1^al Global`N2^be Global`N3^ga Global`N4^de Global`N5^ep)
        ]         ],
             {"["  -> "(",
              "]"  -> ")",
              "\"" ->"",
              "d_(1-x)" -> "DeltaFunction[1-x]"}
                         ] <> ";",
          opt
        ], opt]
) /; ((FORM /. {opt} /. Options[RHI]) === True);

RHI[snum___List,{a_,b_,c_,d_,e_,f_,g_},{al_,be_,ga_,de_,ep_},
    opt___Rule] :=
(
RHI[snum,{a,b,c,d,e,f,g}, {al,be,ga,de,ep}(*, opt*)] =
fromform[
"l M = " <> StringReplace[
ToString[InputForm[
scn[snum]*
pw[Global`d1, a] pw[Global`d2, b] pw[Global`d3, c] *
pw[Global`d4, d] pw[Global`d5, e] pw[Global`d6, f] *
pw[Global`d7, g] /
(Global`N1^al Global`N2^be Global`N3^ga Global`N4^de Global`N5^ep)
        ]         ],
             {"["  -> "(",
              "]"  -> ")",
              "\"" ->"",
              "d_(1-x)" -> "DeltaFunction[1-x]"}
                         ] <> ";",
         opt
        ]
) /; ((FORM /. {opt} /. Options[RHI]) === True);

form00 = {
"#include " <> HomeDirectory[] <> "/rh/ope/dc22",
"#define order \"1\"",
"#define check \"*\"",
"#define comment \"*\""
        };

form0 = {
"#include " <> HomeDirectory[] <> "/rh/ope/dc22",
"#define order \"0\"",
"#define check \"*\"",
"#define comment \"*\""
        };

form1 = {
"#include " <> HomeDirectory[] <> "/rh/ope/dc22",
(*
"#define order \"0\"",
*)
"#define order \"-1\"",
"#define check \"*\"",
"#define comment \"*\""
        };
form2 = {
".sort",
"print;",
"'include'lint",
"'include'yint",
"'include'xint",
".sort",
"print +s;",
".end"
        };

fromform[y_String,opt___Rule] := Block[{formprog, fil,new1,newm,new,fac},
(* construct the form program *)
If[(EpsilonOrder /. {opt} /. Options[RHI]) < 0,
   formprog = Join[form1, {y}, form2],
   If[(EpsilonOrder /. {opt} /. Options[RHI]) === 1,
      formprog = Join[form00, {y}, form2],
      formprog = Join[form0, {y}, form2]
     ]
  ];
fil = HomeDirectory[]<>"/"<>(Directory /. Options[RHI]) <> "doit";
OpenWrite[fil];
For[i = 1, i <= Length[formprog], i++,
    WriteString[fil, formprog[[i]],"\n"];
   ];
Close[fil];
(* run FORM *)
FCPrint[1,"running FORM"];
Run[ HomeDirectory[]<>"/"<> (Directory /. Options[RHI]
                              ) <>"runformdoit"
   ];
FCPrint[1,"FORM done"];
(* read the FORM - result back in *)
ww = ReadList[HomeDirectory[]<>"/rh/ope/diagrams/doit.out", String];
myflag = False;
new = {};

new = Catch[

For[j = 1, j<=Length[ww], j++,
If[StringMatchQ[ww[[j]], "   M = 0;*"], Throw[0]];
    If[myflag === False,
       If[StringMatchQ[ww[[j]], "       +*"] ||
          StringMatchQ[ww[[j]], "       -*"],
          myflag = True
         ],
       If[StringMatchQ[ww[[j]], "      ;"],myflag = False];
      ];
    If[myflag === True,
       AppendTo[new, str[ww[[j]]]];
      ];
   ];
   new = Join[{"("}, new, {")"}];
OpenWrite[HomeDirectory[]<>"/rh/ope/diagrams/doit.m"];
   For[ij = 1, ij <= Length[new], ij++,
       WriteString[HomeDirectory[]<>"/rh/ope/diagrams/doit.m",
                   new[[ij]], "\n"];
      ];
Close[HomeDirectory[]<>"/rh/ope/diagrams/doit.m"];
new = Get[HomeDirectory[]<>"/rh/ope/diagrams/doit.m"];
new = new /. {Global`eph :> ((Epsilon)/2),
                      Global`mark1 :> 1,
                      Global`mark2 :> 1,
                      Global`mark3 :> 1,
                      Global`sn :> 1} ;

new = Factor1[new];
If[Head[new]=!=Times, fac = 1,
   fac = Select2[new, {Global`d0, Global`sn}];
  ];
new = new / fac;
rhd0 = ScalarProduct[OPEDelta , Momentum /. Options[RHI]];
fac = fac /. Global`d0 -> rhd0;
new = Collect2[new, Global`MINUSONE, Factoring -> False];
new = new + null1 + null2;
new1 = Select1[new, Global`MINUSONE] /. {null1:>0, null2:>0};
newm = Select2[new, Global`MINUSONE];

clo[yy__] := If[!FreeQ2[{yy}, {Epsilon,
                               DeltaFunction}], Plus[yy],
               Collect2[Plus[yy], {PolyLog,Log},Factoring->False]
              ];
coled[xx_] := Collect2[xx /. DeltaFunction[_]->0, Epsilon,
            Factoring->False] +
              DeltaFunction[1-Global`x]         Collect2[D[xx,
              DeltaFunction[1-Global`x]], Epsilon,Factoring->False];

new1 = coled[new1] /. Plus -> clo;
If[newm =!= 0,
   mmm  = Select2[newm, Global`MINUSONE];
   newm = newm / mmm,
   newm = 0
  ];
newm = coled[newm] /. Plus -> clo;
newm = newm mmm /. Global`MINUSONE -> (-1);
new = new1 + newm;

new = new fac;
new = new /. Global`m -> (OPEm -2);
new];
new = new /. (-1)^a_ :> PowerSimplify[(-1)^a];
                           new];

str[y_]:=StringReplace[y, {"["        -> "(",
                                  "]" -> ")",
                           "d_(1-x)"  -> "DeltaFunction[1-x]",
                           "(-)^m"    -> "(MINUSONE)^m",
                           "p.p"      -> "ScalarProduct[p,p]",
                           "zeta2"    -> "Zeta2",
                           "zeta3"    -> "Zeta[3]",
                           "s12(1-x)" -> "Nielsen[1,2][1-x]",
                           "li2(1-x)" -> "PolyLog[2,1-x]",
                           "li3(1-x)" -> "PolyLog[3,1-x]",
                           "li3(-x)"  -> "PolyLog[3,-x]",
                           "li3(-(1-x)/(1+x))"  ->
                                  "PolyLog[3,-(1-x)/(1+x)]",
                           "li3((1-x)/(1+x))" ->
                                  "PolyLog[3,(1-x)/(1+x)]",
                           "li2(-x)"  -> "PolyLog[2,-x]",
                           "log(x)"   -> "Log[x]",
                           "log(x,1 - 1/2*x)" -> "LOG[x,1-x/2]",
                           "li2(x,1/2*x)" -> "LI2[x,1/2 x]",
                           "log(1+x)" -> "Log[1+x]",
                           "log(1-x)" -> "Log[1-x]"
                          }
                      ];



pw[x_,y_] := If[Head[y] === Integer,
                frh[x]^y,
(
Global`dm[Global`d1] = Global`dm[Global`k1];
Global`dm[Global`d2] = Global`dm[Global`k2];
Global`dm[Global`d3] = Global`dm[Global`p - Global`k1];
Global`dm[Global`d4] = Global`dm[Global`p - Global`k2];
Global`dm[Global`d5] = Global`dm[Global`k1 - Global`k2];
Global`dm[Global`d6] = Global`dm[Global`p + Global`k1 - Global`k2];
Global`dm[Global`d7] = Global`dm[Global`p - Global`k1 - Global`k2];
);
                Global`dm[x//frh] Cancel[frh[x^y/x^(OPEm-2)]]
               ];


RHI /:
MakeBoxes[RHI[w_,v_], TraditionalForm] :=
 ToBoxes[TLI[w,v],TraditionalForm];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "RHI | \n "]];
Null
