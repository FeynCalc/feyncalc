(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SimplifyGTI *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 4 August '98 at 15:46 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`SimplifyGTI`",{"HighEnergyPhysics`FeynCalc`"}];

SimplifyGTI::"usage"= "SimplifyGTI simplifies GTI's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Epsilon       = MakeContext["CoreObjects","Epsilon"];
GTI           = MakeContext["GTI"];
Momentum      = MakeContext["CoreObjects","Momentum"];
PowerSimplify = MakeContext["PowerSimplify"];
SOD           = MakeContext["CoreObjects","SOD"];
SPD           = MakeContext["CoreObjects","SPD"];

Options[SimplifyGTI] = {Negative -> False};

negflag := Negative /. Options[SimplifyGTI];



eExpand = Identity;
(*
gti[{0,0,x_,y_,z_},{bb__},{cc__}] :> SimplifyGTI[{x,y,z},{bb},{cc}],
*)
(*

*)

(*
(* eq. (3C.20) *)
gti[__, {_,_,_,de_/;de<=0,ep_/;ep<=0}] := 0;
gti[__, {_,_,ga_/;ga<=0,_,ep_/;ep<=0}] := 0;
gti[__, {_,_,ga_/;ga<=0,de_/;de<=0,_}] := 0;
gti[__, {_,be_/;be<=0,_,_,ep_/;ep<=0}] := 0;
gti[__, {_,be_/;be<=0,_,de_/;de<=0,_}] := 0;
gti[__, {al_/;al<=0,_,_,_,ep_/;ep<=0}] := 0;
gti[__, {al_/;al<=0,_,ga_/;ga<=0,_,_}] := 0;
gti[__, {al_/;al<=0,be_/;be<=0,_,_,_}] := 0;
gti[{0, 0, 0},{bb__},{cc__}] := gti[{bb},{cc}];
*)

(* CANCELLING *)

(*
sg[z_] := Expand[z /. gti->gtis];
gtis[z__] := gtis[z] = Expand[gti[z]/.gtirules];
*)
sg[z_] := z /. gti->gtis;
gtis[z__] := gtis[z] = gti[z]/.gtirules/.D->4+Epsilon;
(*
gtis[z__] := gtis[z] = eExpand[gti[z]/.gtirules/.gtirules/.gtirules];
*)

SimplifyGTI[z_] :=
  eExpand[FixedPoint[sg, z /. GTI -> gti, 100000]/.gti->GTI];
(*
SimplifyGTI[z_] := ((z/.GTI->gti) //. gtirules)/.gti->GTI;
*)

PP := PP = SPD[Momentum /. Options[GTI], Momentum /. Options[GTI]];
Dp := Dp = SOD[Momentum /. Options[GTI]];

gti[_] = gti;
(*
gti[_]:>gti /; Global`$gtiFLAG=!=False;
*)

ps[0][z_]  := z;
ps[m_][z_] := eExpand[PowerSimplify[(-1)^m] z] /; m =!= 0;


gtirules=
{
(* eq. (3C.20) *)
gti[__, {_,_,_,de_/;de<=0,ep_/;ep<=0}] :> 0,
gti[__, {_,_,ga_/;ga<=0,_,ep_/;ep<=0}] :> 0,
gti[__, {_,_,ga_/;ga<=0,de_/;de<=0,_}] :> 0,
gti[__, {_,be_/;be<=0,_,_,ep_/;ep<=0}] :> 0,
gti[__, {_,be_/;be<=0,_,de_/;de<=0,_}] :> 0,
gti[__, {al_/;al<=0,_,_,_,ep_/;ep<=0}] :> 0,
gti[__, {al_/;al<=0,_,ga_/;ga<=0,_,_}] :> 0,
gti[__, {al_/;al<=0,be_/;be<=0,_,_,_}] :> 0
,

gti[{0, 0, 0},{bb__},{cc__}]      :> gti[{bb},{cc}]
,
(* MOVING *)
(* 0,-1, ...  away from alpha *)
(*G0*)
gti[{a_,b_,c_,d_,e_},{al_/;al<=0,be_, ga_, de_/;de>0, ep_}
   ] :> gti[0][{d,c,b,a,e},{de,ga,be,al,ep}]
,

(*G1*)
gti[{a_,b_,c_,d_,e_},{al_/;al<=0,be_, ga_, de_/;de<0, ep_}
   ] :> gti[1][{d,c,b,a,e},{de,ga,be,al,ep}] /; al > de
,

(* 0,-1, ...  away from beta *)
(* k2->-k2+p; k1->-k1+p *)
(*G2*)
gti[{a_,b_,c_,d_,e_},{al_,be_/;be<=0,ga_/;ga=!=0,de_/;de>0, ep_}
   ] :> ps[e][gti[2][{c,d,a,b,e},{ga,de,al,be,ep}]]
,

(*G3*) (* k1 <--> k2 *)
gti[{a_Integer, b_Integer, c_, d_, e_}, {al_,be_,ga_/;ga<=0,de_/;de>=0,ep_}
   ] :> ps[e][gti[3][{b,a,d,c,e},{be,al,de,ga,ep}]] /; b>a
,

(*G4*)
gti[{a_Integer, b_/;Head[b]=!=Integer, c_, d_, e_},
    {al_,be_,ga_,de_/;de>0,ep_/;ep>0}
   ] :> ps[e][gti[4][{b,a,d,c,e},{be,al,de,ga,ep}]]
,

(*G5*) (* k1 <--> k2 *) (* f441*)
gti[{a_,b_,c_,d_,e_},{al_,be_,0,de_ /; de=!=0, ep_}
   ] :> ps[e][gti[5][{b, a, d, c, e}, {be, al, de, 0, ep}]]
,
gti[{a_,b_,c_,d_,e_},{al_,be_,ga_,de_,0}] :>
 ps[e][gti[{b,a,d,c,e},{be,al,de,ga,0}]] /; !OrderedQ[
  { {{b,a,d,c,e},{be,al,de,ga,0}},
   {{a,b,c,d,e},{al,be,ga,de,0}}
  }]
,

(*G6*)
gti[{a_,b_,c_,d_,e_},{al_,be_/;be>0,ga_Integer?Negative, de_ /; de>0, ep_}
   ] :> ps[e][gti[6][{b, a, d, c, e}, {be, al, de, ga, ep}]]
,

(*G7*) (* f442*)
gti[{a_,b_Integer,c_,0,e_ /; Head[e]=!=Integer},
    {al_,be_,ga_,0,  ep_}
   ] :> gti[7][{a,e,c,0,b},{al,ep,ga,0,be}]
,
(*G8*)
gti[{a_,b_,c_,d_Integer?Negative,0},
    {0,be_Integer?Positive, ga_Integer?Positive,0,ep_Integer?Positive}
   ] :> gti[{d,c,b,a,0},{0,ga,be,0,ep}]
,

(* FINITE *)

gti[{a_Integer, b_, c_, d_, e_},
    {al_/;al>0, be_/;be>0,ga_/;ga>0,de_/;de>0,ep_/;ep>0}
   ]:> ( ps[e][gti[{c,d,a,b,e}, {ga,de,al,be,ep}]]
       ) /; (Head[c]=!=Integer || c > a)
,

gti[{a_Integer, b_, c_, d_, e_},
    {al_/;al>0, be_/;be>0,ga_/;ga>0,de_/;de>0,ep_/;ep>0}
   ]:> ( ps[e][gti[{b,a,d,c,e},{be,al,de,ga,ep}]]
       ) /; ((Head[b]=!=Integer) || (b > a))
,

(* NONFINITE *)
(*SUBLOOP*)
gti[{a_,0, c_, 0, 0},{1,1,1,1,-1}
   ]  :> -1/2 PP gti[{a,0,c,0,0},{1,1,1,1,0}]
,

gti[{0,b_, 0, d_, 0},{1,1,1,1,-1}
   ]  :> -1/2 PP gti[{0,b,0,d,0},{1,1,1,1,0}]
,

gti[{a_,0, c_, 0, 0},{1,1,1,1,-2}
   ]  :> 4 (PP^2/16 + PP^2/(16*(3 + Epsilon))
         ) gti[{a,0,c,0,0},{1,1,1,1,0}]
,

gti[{0,b_, 0, d_, 0},{1,1,1,1,-2}
   ]  :> 4 (PP^2/16 + PP^2/(16*(3 + Epsilon))
         ) gti[{0,b,0,d,0},{1,1,1,1,0}]
,

gti[{a_,0, c_, 0, 0},{1,1,1,1,-3}
   ]  :> -8 (((6 + Epsilon)*PP^3)/(64*(3 + Epsilon))
             ) gti[{a,0,c,0,0},{1,1,1,1,0}]
,

gti[{0, b_, 0, d_, 0, 0},{1,1,1,1,-3}
   ]  :> -8 (((6 + Epsilon)*PP^3)/(64*(3 + Epsilon))
             ) gti[{0,b,0,d,0},{1,1,1,1,0}]
,

gti[{a_,b_,c_,d_,e_},{1,1,1,1,-1}
   ]  :> (2 PP/Dp^2 gti[{a+1,b+1,c,d,e},{1,1,1,1,0}] -
            PP/Dp gti[{a,b+1,c,d,e},{1,1,1,1,0}] -
            PP/Dp gti[{a+1,b,c,d,e},{1,1,1,1,0}]
         ) /; ( (a=!=0 && b=!=0) || (c=!=0 && d=!=0) || e=!=0)
,

gti[{a_,b_,c_,d_,e_},{1,1,1,1,-2}
   ] :> 4 (
(PP^2*gti[{a, 2 + b, c, d, e}, {1, 1, 1, 1, 0}])/(4*Dp^2) +
  (PP^2*gti[{1 + a, 1 + b, c, d, e}, {1, 1, 1, 1, 0}])/(2*Dp^2) +
  (PP^2*gti[{1 + a, 1 + b, c, d, e}, {1, 1, 1, 1, 0}])/(Dp^2*(2 + Epsilon)) -
  (PP^2*gti[{1 + a, 2 + b, c, d, e}, {1, 1, 1, 1, 0}])/Dp^3 -
  (PP^2*gti[{1 + a, 2 + b, c, d, e}, {1, 1, 1, 1, 0}])/(Dp^3*(2 + Epsilon)) +
  (PP^2*gti[{2 + a, b, c, d, e}, {1, 1, 1, 1, 0}])/(4*Dp^2) -
  (PP^2*gti[{2 + a, 1 + b, c, d, e}, {1, 1, 1, 1, 0}])/Dp^3 -
  (PP^2*gti[{2 + a, 1 + b, c, d, e}, {1, 1, 1, 1, 0}])/(Dp^3*(2 + Epsilon)) +
  (PP^2*gti[{2 + a, 2 + b, c, d, e}, {1, 1, 1, 1, 0}])/Dp^4 +
  (PP^2*gti[{2 + a, 2 + b, c, d, e}, {1, 1, 1, 1, 0}])/(Dp^4*(2 + Epsilon))
         ) /; ( (a=!=0 && b=!=0) || (c=!=0 && d=!=0) || (e=!=0) ||
                (b=!=0 && c=!=0) || (a=!=0 && d=!=0)
              )
,

gti[{a_, b_, c_, d_, e_}, {1, 1, 1, 1, -1}
   ] :>
(
  (a*Dp*gti[{-1 + a, b, c, d, e}, {1, 1, 1, 1, -1}] -
     a*PP*gti[{-1 + a, b, c, d, 1 + e}, {1, 1, 1, 1, 0}] -
     2*PP*gti[{a, b, c, d, e}, {1, 1, 1, 1, 0}] -
     Epsilon*PP*gti[{a, b, c, d, e}, {1, 1, 1, 1, 0}]
  )/(2*(2 + a + Epsilon))
)

,
(*CHANGE01
gti[{a_/;a>=0, b_, c_/;c>=0, d_, e_},
*)
gti[{a_/;a>=0, b_, c_/;c>=0, d_, e_/;e>=0},
    {al_, be_/;be>0, ga_, de_/;de>0, ep_/;ep>0}] :>
(  -((a*gti[{-1 + a, b, c, d, 1 + e}, {al, be, ga, de, ep}] -
       c*gti[{a, b, -1 + c, d, 1 + e}, {al, be, ga, de, ep}] +
       ga*gti[{a, b, c, d, e}, {al, be, 1 + ga, -1 + de, ep}] -
       ga*gti[{a, b, c, d, e}, {al, be, 1 + ga, de, -1 + ep}] +
       al*gti[{a, b, c, d, e}, {1 + al, -1 + be, ga, de, ep}] -
       al*gti[{a, b, c, d, e}, {1 + al, be, ga, de, -1 + ep}])/
     (4 - al + e - 2*ep + Epsilon - ga))
)
,
(* CHANGE *)
gti[{a_, b_/;b>=0, c_, d_/;d>=0, e_ /; e>=0},
    {al_, be_/;be>0(*/;be<0*), ga_/;ga>0, de_/;de>0, ep_/;ep>0}] :>
(
  (b*gti[{a, -1 + b, c, d, 1 + e}, {al, be, ga, de, ep}] -
     d*gti[{a, b, c, -1 + d, 1 + e}, {al, be, ga, de, ep}] -
     be*gti[{a, b, c, d, e}, {-1 + al, 1 + be, ga, de, ep}] -
     de*gti[{a, b, c, d, e}, {al, be, -1 + ga, 1 + de, ep}] +
     de*gti[{a, b, c, d, e}, {al, be, ga, 1 + de, -1 + ep}] +
     be*gti[{a, b, c, d, e}, {al, 1 + be, ga, de, -1 + ep}])/
   (4 - be - de + e - 2*ep + Epsilon)
)
,
(* NO OTHER WAY, OR? *)
gti[{a_, b_/;b>=0, c_, d_/;d>=0, e_/;Head[e] =!= Integer},
    {al_/;al>0, be_/;be>0(*/;be<0*), ga_/;ga>0, de_/;de>0, ep_/;ep>0}] :>
(
  (b*gti[{a, -1 + b, c, d, 1 + e}, {al, be, ga, de, ep}] -
     d*gti[{a, b, c, -1 + d, 1 + e}, {al, be, ga, de, ep}] -
     be*gti[{a, b, c, d, e}, {-1 + al, 1 + be, ga, de, ep}] -
     de*gti[{a, b, c, d, e}, {al, be, -1 + ga, 1 + de, ep}] +
     de*gti[{a, b, c, d, e}, {al, be, ga, 1 + de, -1 + ep}] +
     be*gti[{a, b, c, d, e}, {al, 1 + be, ga, de, -1 + ep}])/
   (4 - be - de + e - 2*ep + Epsilon)
)
,
(*
(*rg21*) (* Delta up if ep>1 *)
gti[{a_, b_, c_, d_, e_}, {al_, be_, ga_, de_/;de<0, ep_/;ep>1}] :>
   -((a*Dp*gti[{-1 + a, b, c, d, e}, {al, be, ga, 1 + de, -1 + ep}] -
        c*Dp*gti[{a, b, -1 + c, d, e}, {al, be, ga, 1 + de, -1 + ep}] +
        Dp*e*gti[{a, b, c, d, -1 + e}, {al, be, ga, 1 + de, -1 + ep}] -
        (1 - ep)*gti[{a, b, c, d, e}, {al, be, -1 + ga, 1 + de, ep}] -
        (5 + a - al - ep + Epsilon - 2*ga)*
         gti[{a, b, c, d, e}, {al, be, ga, 1 + de, -1 + ep}] +
        al*gti[{a, b, c, d, e}, {1 + al, be, -1 + ga, 1 + de, -1 + ep}] -
        al*PP*gti[{a, b, c, d, e}, {1 + al, be, ga, 1 + de, -1 + ep}] +
        c*gti[{1 + a, b, -1 + c, d, e}, {al, be, ga, 1 + de, -1 + ep}] -
        e*gti[{1 + a, b, c, d, -1 + e}, {al, be, ga, 1 + de, -1 + ep}])/
      (1 - ep))
,
*)
(*XXX*)

(*"RR1"*)
gti[{a_, 0, c_, 0, 0}, {al_, be_, ga_, de_, -1}] :>
    -gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, ga, de, 0}]/(2*PP) +
     gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, -1 + de, 0}]/(2*PP) +
     gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, de, 0}]/2 +
     gti[{a, 0, c, 0, 0}, {al, -1 + be, -1 + ga, de, 0}]/(2*PP) +
     gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, de, 0}]/2 -
     gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, -1 + de, 0}]/(2*PP) +
     gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, de, 0}]/2 +
     gti[{a, 0, c, 0, 0}, {al, be, ga, -1 + de, 0}]/2 -
     (PP*gti[{a, 0, c, 0, 0}, {al, be, ga, de, 0}])/2
,
(*"RR2"*)
gti[{0, b_, 0, d_, 0}, {al_, be_, ga_, de_, -1}] :>
    -gti[{0, b, 0, d, 0}, {-1 + al, -1 + be, ga, de, 0}]/(2*PP) +
     gti[{0, b, 0, d, 0}, {-1 + al, be, ga, -1 + de, 0}]/(2*PP) +
     gti[{0, b, 0, d, 0}, {-1 + al, be, ga, de, 0}]/2 +
     gti[{0, b, 0, d, 0}, {al, -1 + be, -1 + ga, de, 0}]/(2*PP) +
     gti[{0, b, 0, d, 0}, {al, -1 + be, ga, de, 0}]/2 -
     gti[{0, b, 0, d, 0}, {al, be, -1 + ga, -1 + de, 0}]/(2*PP) +
     gti[{0, b, 0, d, 0}, {al, be, -1 + ga, de, 0}]/2 +
     gti[{0, b, 0, d, 0}, {al, be, ga, -1 + de, 0}]/2 -
     (PP*gti[{0, b, 0, d, 0}, {al, be, ga, de, 0}])/2
,
(*"RR3"*)
gti[{a_, b_, c_, d_, e_}, {al_, be_, ga_, de_, -1}] :>
    gti[{a, b, c, d, e}, {-1 + al, be, ga, de, 0}] +
      gti[{a, b, c, d, e}, {al, -1 + be, ga, de, 0}] -
      gti[{a, 1 + b, c, d, e}, {-1 + al, be, ga, de, 0}]/Dp +
      gti[{a, 1 + b, c, d, e}, {al, be, -1 + ga, de, 0}]/Dp -
      (PP*gti[{a, 1 + b, c, d, e}, {al, be, ga, de, 0}])/Dp -
      gti[{1 + a, b, c, d, e}, {al, -1 + be, ga, de, 0}]/Dp +
      gti[{1 + a, b, c, d, e}, {al, be, ga, -1 + de, 0}]/Dp -
      (PP*gti[{1 + a, b, c, d, e}, {al, be, ga, de, 0}])/Dp +
      (2*PP*gti[{1 + a, 1 + b, c, d, e}, {al, be, ga, de, 0}])/Dp^2 /;
     (b =!= 0) || (d =!= 0) || (e =!= 0)
,
(*"RR4"*)
gti[{a_, 0, c_, 0, 0}, {al_, be_, ga_, de_, -2}] :>
    -(D*gti[{a, 0, c, 0, 0}, {-2 + al, -2 + be, ga, de, 0}])/
      (4*(1 - D)*PP^2) + (D*gti[{a, 0, c, 0, 0},
         {-2 + al, -1 + be, ga, -1 + de, 0}])/(2*(1 - D)*PP^2) +
     ((2 - D)*gti[{a, 0, c, 0, 0}, {-2 + al, -1 + be, ga, de, 0}])/
      (2*(1 - D)*PP) - (D*gti[{a, 0, c, 0, 0},
         {-2 + al, be, ga, -2 + de, 0}])/(4*(1 - D)*PP^2) +
     (D*gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, -1 + de, 0}])/
      (2*(1 - D)*PP) - ((4 - 3*D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, de, 0}])/(4*(1 - D)) +
     (D*gti[{a, 0, c, 0, 0}, {-1 + al, -2 + be, -1 + ga, de, 0}])/
      (2*(1 - D)*PP^2) + ((2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -2 + be, ga, de, 0}])/(2*(1 - D)*PP) \
- (D*gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, -1 + ga, -1 + de, 0}])/
      ((1 - D)*PP^2) - ((2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, -1 + ga, de, 0}])/
      ((1 - D)*PP) - ((2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, ga, -1 + de, 0}])/
      ((1 - D)*PP) - ((2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, ga, de, 0}])/(1 - D) +
     (D*gti[{a, 0, c, 0, 0}, {-1 + al, be, -1 + ga, -2 + de, 0}])/
      (2*(1 - D)*PP^2) - (D*gti[{a, 0, c, 0, 0},
         {-1 + al, be, -1 + ga, -1 + de, 0}])/((1 - D)*PP) +
     (D*gti[{a, 0, c, 0, 0}, {-1 + al, be, -1 + ga, de, 0}])/(2*(1 - D)) +
     ((2 - D)*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, -2 + de, 0}])/
      (2*(1 - D)*PP) - ((2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, -1 + de, 0}])/(1 - D) +
     2*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, de, -1}] +
     ((2 - D)*PP*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, de, 0}])/
      (2*(1 - D)) - (D*gti[{a, 0, c, 0, 0},
         {al, -2 + be, -2 + ga, de, 0}])/(4*(1 - D)*PP^2) +
     (D*gti[{a, 0, c, 0, 0}, {al, -2 + be, -1 + ga, de, 0}])/
      (2*(1 - D)*PP) - ((4 - 3*D)*
        gti[{a, 0, c, 0, 0}, {al, -2 + be, ga, de, 0}])/(4*(1 - D)) +
     (D*gti[{a, 0, c, 0, 0}, {al, -1 + be, -2 + ga, -1 + de, 0}])/
      (2*(1 - D)*PP^2) + ((2 - D)*
        gti[{a, 0, c, 0, 0}, {al, -1 + be, -2 + ga, de, 0}])/(2*(1 - D)*PP) \
- (D*gti[{a, 0, c, 0, 0}, {al, -1 + be, -1 + ga, -1 + de, 0}])/
      ((1 - D)*PP) - ((2 - D)*
        gti[{a, 0, c, 0, 0}, {al, -1 + be, -1 + ga, de, 0}])/(1 - D) +
     (D*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, -1 + de, 0}])/(2*(1 - D)) +
     2*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, de, -1}] +
     ((2 - D)*PP*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, de, 0}])/
      (2*(1 - D)) - (D*gti[{a, 0, c, 0, 0},
         {al, be, -2 + ga, -2 + de, 0}])/(4*(1 - D)*PP^2) +
     (D*gti[{a, 0, c, 0, 0}, {al, be, -2 + ga, -1 + de, 0}])/
      (2*(1 - D)*PP) - (D*gti[{a, 0, c, 0, 0}, {al, be, -2 + ga, de, 0}])/
      (4*(1 - D)) + (D*gti[{a, 0, c, 0, 0},
         {al, be, -1 + ga, -2 + de, 0}])/(2*(1 - D)*PP) -
     (D*gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, -1 + de, 0}])/(1 - D) +
     (D*PP*gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, de, 0}])/(2*(1 - D)) -
     (D*gti[{a, 0, c, 0, 0}, {al, be, ga, -2 + de, 0}])/(4*(1 - D)) +
     (D*PP*gti[{a, 0, c, 0, 0}, {al, be, ga, -1 + de, 0}])/(2*(1 - D)) -
     (D*PP^2*gti[{a, 0, c, 0, 0}, {al, be, ga, de, 0}])/(4*(1 - D))
,
(*"RR5"*)
gti[{0, b_, 0, d_, 0}, {al_, be_, ga_, de_, -2}] :>
    -(D*gti[{0, b, 0, d, 0}, {-2 + al, -2 + be, ga, de, 0}])/
      (4*(1 - D)*PP^2) + (D*gti[{0, b, 0, d, 0},
         {-2 + al, -1 + be, ga, -1 + de, 0}])/(2*(1 - D)*PP^2) +
     ((2 - D)*gti[{0, b, 0, d, 0}, {-2 + al, -1 + be, ga, de, 0}])/
      (2*(1 - D)*PP) - (D*gti[{0, b, 0, d, 0},
         {-2 + al, be, ga, -2 + de, 0}])/(4*(1 - D)*PP^2) +
     (D*gti[{0, b, 0, d, 0}, {-2 + al, be, ga, -1 + de, 0}])/
      (2*(1 - D)*PP) - ((4 - 3*D)*
        gti[{0, b, 0, d, 0}, {-2 + al, be, ga, de, 0}])/(4*(1 - D)) +
     (D*gti[{0, b, 0, d, 0}, {-1 + al, -2 + be, -1 + ga, de, 0}])/
      (2*(1 - D)*PP^2) + ((2 - D)*
        gti[{0, b, 0, d, 0}, {-1 + al, -2 + be, ga, de, 0}])/(2*(1 - D)*PP) \
- (D*gti[{0, b, 0, d, 0}, {-1 + al, -1 + be, -1 + ga, -1 + de, 0}])/
      ((1 - D)*PP^2) - ((2 - D)*
        gti[{0, b, 0, d, 0}, {-1 + al, -1 + be, -1 + ga, de, 0}])/
      ((1 - D)*PP) - ((2 - D)*gti[{0, b, 0, d, 0},
         {-1 + al, -1 + be, ga, -1 + de, 0}])/((1 - D)*PP) -
     ((2 - D)*gti[{0, b, 0, d, 0}, {-1 + al, -1 + be, ga, de, 0}])/(1 - D) +
     (D*gti[{0, b, 0, d, 0}, {-1 + al, be, -1 + ga, -2 + de, 0}])/
      (2*(1 - D)*PP^2) - (D*gti[{0, b, 0, d, 0},
         {-1 + al, be, -1 + ga, -1 + de, 0}])/((1 - D)*PP) +
     (D*gti[{0, b, 0, d, 0}, {-1 + al, be, -1 + ga, de, 0}])/(2*(1 - D)) +
     ((2 - D)*gti[{0, b, 0, d, 0}, {-1 + al, be, ga, -2 + de, 0}])/
      (2*(1 - D)*PP) - ((2 - D)*
        gti[{0, b, 0, d, 0}, {-1 + al, be, ga, -1 + de, 0}])/(1 - D) +
     2*gti[{0, b, 0, d, 0}, {-1 + al, be, ga, de, -1}] +
     ((2 - D)*PP*gti[{0, b, 0, d, 0}, {-1 + al, be, ga, de, 0}])/
      (2*(1 - D)) - (D*gti[{0, b, 0, d, 0}, {al, -2 + be, -2 + ga, de, 0}])/
      (4*(1 - D)*PP^2) + (D*gti[{0, b, 0, d, 0},
         {al, -2 + be, -1 + ga, de, 0}])/(2*(1 - D)*PP) -
     ((4 - 3*D)*gti[{0, b, 0, d, 0}, {al, -2 + be, ga, de, 0}])/
      (4*(1 - D)) + (D*gti[{0, b, 0, d, 0},
         {al, -1 + be, -2 + ga, -1 + de, 0}])/(2*(1 - D)*PP^2) +
     ((2 - D)*gti[{0, b, 0, d, 0}, {al, -1 + be, -2 + ga, de, 0}])/
      (2*(1 - D)*PP) - (D*gti[{0, b, 0, d, 0},
         {al, -1 + be, -1 + ga, -1 + de, 0}])/((1 - D)*PP) -
     ((2 - D)*gti[{0, b, 0, d, 0}, {al, -1 + be, -1 + ga, de, 0}])/(1 - D) +
     (D*gti[{0, b, 0, d, 0}, {al, -1 + be, ga, -1 + de, 0}])/(2*(1 - D)) +
     2*gti[{0, b, 0, d, 0}, {al, -1 + be, ga, de, -1}] +
     ((2 - D)*PP*gti[{0, b, 0, d, 0}, {al, -1 + be, ga, de, 0}])/
      (2*(1 - D)) - (D*gti[{0, b, 0, d, 0}, {al, be, -2 + ga, -2 + de, 0}])/
      (4*(1 - D)*PP^2) + (D*gti[{0, b, 0, d, 0},
         {al, be, -2 + ga, -1 + de, 0}])/(2*(1 - D)*PP) -
     (D*gti[{0, b, 0, d, 0}, {al, be, -2 + ga, de, 0}])/(4*(1 - D)) +
     (D*gti[{0, b, 0, d, 0}, {al, be, -1 + ga, -2 + de, 0}])/
      (2*(1 - D)*PP) - (D*gti[{0, b, 0, d, 0},
         {al, be, -1 + ga, -1 + de, 0}])/(1 - D) +
     (D*PP*gti[{0, b, 0, d, 0}, {al, be, -1 + ga, de, 0}])/(2*(1 - D)) -
     (D*gti[{0, b, 0, d, 0}, {al, be, ga, -2 + de, 0}])/(4*(1 - D)) +
     (D*PP*gti[{0, b, 0, d, 0}, {al, be, ga, -1 + de, 0}])/(2*(1 - D)) -
     (D*PP^2*gti[{0, b, 0, d, 0}, {al, be, ga, de, 0}])/(4*(1 - D))
,
(*RR6*)
gti[{a_, b_, c_, d_, e_}, {al_, be_, ga_, de_, -2}] :>
    -(D*gti[{a, b, c, d, e}, {-2 + al, -2 + be, ga, de, 0}])/
       (4*(1 - D)*PP^2) + (D*gti[{a, b, c, d, e},
          {-2 + al, -1 + be, ga, -1 + de, 0}])/(2*(1 - D)*PP^2) +
      ((2 - D)*gti[{a, b, c, d, e}, {-2 + al, -1 + be, ga, de, 0}])/
       (2*(1 - D)*PP) - (D*gti[{a, b, c, d, e},
          {-2 + al, be, ga, -2 + de, 0}])/(4*(1 - D)*PP^2) +
      (D*gti[{a, b, c, d, e}, {-2 + al, be, ga, -1 + de, 0}])/
       (2*(1 - D)*PP) - ((4 - 3*D)*
         gti[{a, b, c, d, e}, {-2 + al, be, ga, de, 0}])/(4*(1 - D)) +
      (D*gti[{a, b, c, d, e}, {-1 + al, -2 + be, -1 + ga, de, 0}])/
       (2*(1 - D)*PP^2) + ((2 - D)*
         gti[{a, b, c, d, e}, {-1 + al, -2 + be, ga, de, 0}])/
       (2*(1 - D)*PP) - (D*gti[{a, b, c, d, e},
          {-1 + al, -1 + be, -1 + ga, -1 + de, 0}])/((1 - D)*PP^2) -
      ((2 - D)*gti[{a, b, c, d, e}, {-1 + al, -1 + be, -1 + ga, de, 0}])/
       ((1 - D)*PP) - ((2 - D)*
         gti[{a, b, c, d, e}, {-1 + al, -1 + be, ga, -1 + de, 0}])/
       ((1 - D)*PP) - ((2 - D)*
         gti[{a, b, c, d, e}, {-1 + al, -1 + be, ga, de, 0}])/(1 - D) +
      (D*gti[{a, b, c, d, e}, {-1 + al, be, -1 + ga, -2 + de, 0}])/
       (2*(1 - D)*PP^2) - (D*gti[{a, b, c, d, e},
          {-1 + al, be, -1 + ga, -1 + de, 0}])/((1 - D)*PP) +
      (D*gti[{a, b, c, d, e}, {-1 + al, be, -1 + ga, de, 0}])/
       (2*(1 - D)) + ((2 - D)*
         gti[{a, b, c, d, e}, {-1 + al, be, ga, -2 + de, 0}])/
       (2*(1 - D)*PP) - ((2 - D)*
         gti[{a, b, c, d, e}, {-1 + al, be, ga, -1 + de, 0}])/(1 - D) +
      2*gti[{a, b, c, d, e}, {-1 + al, be, ga, de, -1}] +
      ((2 - D)*PP*gti[{a, b, c, d, e}, {-1 + al, be, ga, de, 0}])/
       (2*(1 - D)) - (D*gti[{a, b, c, d, e},
          {al, -2 + be, -2 + ga, de, 0}])/(4*(1 - D)*PP^2) +
      (D*gti[{a, b, c, d, e}, {al, -2 + be, -1 + ga, de, 0}])/
       (2*(1 - D)*PP) - ((4 - 3*D)*
         gti[{a, b, c, d, e}, {al, -2 + be, ga, de, 0}])/(4*(1 - D)) +
      (D*gti[{a, b, c, d, e}, {al, -1 + be, -2 + ga, -1 + de, 0}])/
       (2*(1 - D)*PP^2) + ((2 - D)*
         gti[{a, b, c, d, e}, {al, -1 + be, -2 + ga, de, 0}])/
       (2*(1 - D)*PP) - (D*gti[{a, b, c, d, e},
          {al, -1 + be, -1 + ga, -1 + de, 0}])/((1 - D)*PP) -
      ((2 - D)*gti[{a, b, c, d, e}, {al, -1 + be, -1 + ga, de, 0}])/
       (1 - D) + (D*gti[{a, b, c, d, e}, {al, -1 + be, ga, -1 + de, 0}])/
       (2*(1 - D)) + 2*gti[{a, b, c, d, e}, {al, -1 + be, ga, de, -1}] +
      ((2 - D)*PP*gti[{a, b, c, d, e}, {al, -1 + be, ga, de, 0}])/
       (2*(1 - D)) - (D*gti[{a, b, c, d, e},
          {al, be, -2 + ga, -2 + de, 0}])/(4*(1 - D)*PP^2) +
      (D*gti[{a, b, c, d, e}, {al, be, -2 + ga, -1 + de, 0}])/
       (2*(1 - D)*PP) - (D*gti[{a, b, c, d, e},
          {al, be, -2 + ga, de, 0}])/(4*(1 - D)) +
      (D*gti[{a, b, c, d, e}, {al, be, -1 + ga, -2 + de, 0}])/
       (2*(1 - D)*PP) - (D*gti[{a, b, c, d, e},
          {al, be, -1 + ga, -1 + de, 0}])/(1 - D) +
      (D*PP*gti[{a, b, c, d, e}, {al, be, -1 + ga, de, 0}])/(2*(1 - D)) -
      (D*gti[{a, b, c, d, e}, {al, be, ga, -2 + de, 0}])/(4*(1 - D)) +
      (D*PP*gti[{a, b, c, d, e}, {al, be, ga, -1 + de, 0}])/(2*(1 - D)) -
      (D*PP^2*gti[{a, b, c, d, e}, {al, be, ga, de, 0}])/(4*(1 - D)) /;
     (b =!= 0) || (d =!= 0) || (e =!= 0)
,
(*RR7*)
gti[{a_, 0, c_, 0, 0}, {al_, be_, ga_, de_, -3}] :>
    ((2 + D)*gti[{a, 0, c, 0, 0}, {-3 + al, -3 + be, ga, de, 0}])/
      (8*(1 - D)*PP^3) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-3 + al, -2 + be, ga, -1 + de, 0}])/
      (8*(1 - D)*PP^3) - (3*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-3 + al, -2 + be, ga, de, 0}])/
      (8*(1 - D)*PP^2) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-3 + al, -1 + be, ga, -2 + de, 0}])/
      (8*(1 - D)*PP^3) - (3*D*
        gti[{a, 0, c, 0, 0}, {-3 + al, -1 + be, ga, -1 + de, 0}])/
      (4*(1 - D)*PP^2) - (3*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-3 + al, -1 + be, ga, de, 0}])/(8*(1 - D)*PP) \
- ((2 + D)*gti[{a, 0, c, 0, 0}, {-3 + al, be, ga, -3 + de, 0}])/
      (8*(1 - D)*PP^3) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-3 + al, be, ga, -2 + de, 0}])/
      (8*(1 - D)*PP^2) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-3 + al, be, ga, -1 + de, 0}])/(8*(1 - D)*PP) \
+ ((10 - 7*D)*gti[{a, 0, c, 0, 0}, {-3 + al, be, ga, de, 0}])/(8*(1 - D)) -
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {-2 + al, -3 + be, -1 + ga, de, 0}])/
      (8*(1 - D)*PP^3) - (3*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, -3 + be, ga, de, 0}])/
      (8*(1 - D)*PP^2) + (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, -2 + be, -1 + ga, -1 + de, 0}])/
      (8*(1 - D)*PP^3) + (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, -2 + be, -1 + ga, de, 0}])/
      (8*(1 - D)*PP^2) + (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, -2 + be, ga, -1 + de, 0}])/
      (8*(1 - D)*PP^2) - (3*(2 - 3*D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, -2 + be, ga, de, 0}])/(8*(1 - D)*PP) \
- (9*(2 + D)*gti[{a, 0, c, 0, 0},
         {-2 + al, -1 + be, -1 + ga, -2 + de, 0}])/(8*(1 - D)*PP^3) +
     (9*D*gti[{a, 0, c, 0, 0}, {-2 + al, -1 + be, -1 + ga, -1 + de, 0}])/
      (4*(1 - D)*PP^2) + (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, -1 + be, -1 + ga, de, 0}])/
      (8*(1 - D)*PP) - (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, -1 + be, ga, -2 + de, 0}])/
      (8*(1 - D)*PP^2) + (3*(4 - 3*D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, -1 + be, ga, -1 + de, 0}])/
      (4*(1 - D)*PP) + (3*(6 - 5*D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, -1 + be, ga, de, 0}])/(8*(1 - D)) +
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {-2 + al, be, -1 + ga, -3 + de, 0}])/
      (8*(1 - D)*PP^3) - (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, be, -1 + ga, -2 + de, 0}])/
      (8*(1 - D)*PP^2) + (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, be, -1 + ga, -1 + de, 0}])/
      (8*(1 - D)*PP) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, be, -1 + ga, de, 0}])/(8*(1 - D)) +
     (3*(2 - D)*gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, -3 + de, 0}])/
      (8*(1 - D)*PP^2) - (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, -2 + de, 0}])/(8*(1 - D)*PP) \
+ (9*(2 - D)*gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, -1 + de, 0}])/
      (8*(1 - D)) - 3*gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, de, -1}] -
     (3*(2 - D)*PP*gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, de, 0}])/
      (8*(1 - D)) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -3 + be, -2 + ga, de, 0}])/
      (8*(1 - D)*PP^3) - (3*D*
        gti[{a, 0, c, 0, 0}, {-1 + al, -3 + be, -1 + ga, de, 0}])/
      (4*(1 - D)*PP^2) - (3*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -3 + be, ga, de, 0}])/(8*(1 - D)*PP) \
- (9*(2 + D)*gti[{a, 0, c, 0, 0},
         {-1 + al, -2 + be, -2 + ga, -1 + de, 0}])/(8*(1 - D)*PP^3) -
     (9*(2 - D)*gti[{a, 0, c, 0, 0}, {-1 + al, -2 + be, -2 + ga, de, 0}])/
      (8*(1 - D)*PP^2) + (9*D*
        gti[{a, 0, c, 0, 0}, {-1 + al, -2 + be, -1 + ga, -1 + de, 0}])/
      (4*(1 - D)*PP^2) + (3*(4 - 3*D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -2 + be, -1 + ga, de, 0}])/
      (4*(1 - D)*PP) + (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -2 + be, ga, -1 + de, 0}])/
      (8*(1 - D)*PP) + (3*(6 - 5*D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -2 + be, ga, de, 0}])/(8*(1 - D)) +
     (9*(2 + D)*gti[{a, 0, c, 0, 0},
         {-1 + al, -1 + be, -2 + ga, -2 + de, 0}])/(8*(1 - D)*PP^3) -
     (9*D*gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, -2 + ga, -1 + de, 0}])/
      (4*(1 - D)*PP^2) - (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, -2 + ga, de, 0}])/
      (8*(1 - D)*PP) - (9*D*gti[{a, 0, c, 0, 0},
         {-1 + al, -1 + be, -1 + ga, -2 + de, 0}])/(4*(1 - D)*PP^2) -
     (3*(2 - 3*D)*gti[{a, 0, c, 0, 0},
         {-1 + al, -1 + be, -1 + ga, -1 + de, 0}])/(2*(1 - D)*PP) +
     (3*(4 - 3*D)*gti[{a, 0, c, 0, 0},
         {-1 + al, -1 + be, -1 + ga, de, 0}])/(4*(1 - D)) -
     (9*(2 - D)*gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, ga, -2 + de, 0}])/
      (8*(1 - D)*PP) + (3*(4 - 3*D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, ga, -1 + de, 0}])/
      (4*(1 - D)) - 6*gti[{a, 0, c, 0, 0},
       {-1 + al, -1 + be, ga, de, -1}] -
     (3*(2 - 3*D)*PP*gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, ga, de, 0}])/
      (8*(1 - D)) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, -2 + ga, -3 + de, 0}])/
      (8*(1 - D)*PP^3) + (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, -2 + ga, -2 + de, 0}])/
      (8*(1 - D)*PP^2) - (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, -2 + ga, -1 + de, 0}])/
      (8*(1 - D)*PP) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, -2 + ga, de, 0}])/(8*(1 - D)) +
     (3*D*gti[{a, 0, c, 0, 0}, {-1 + al, be, -1 + ga, -3 + de, 0}])/
      (4*(1 - D)*PP^2) - (9*D*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, -1 + ga, -2 + de, 0}])/
      (4*(1 - D)*PP) + (9*D*gti[{a, 0, c, 0, 0},
         {-1 + al, be, -1 + ga, -1 + de, 0}])/(4*(1 - D)) -
     (3*D*PP*gti[{a, 0, c, 0, 0}, {-1 + al, be, -1 + ga, de, 0}])/
      (4*(1 - D)) + (3*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, -3 + de, 0}])/(8*(1 - D)*PP) \
- (9*(2 - D)*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, -2 + de, 0}])/
      (8*(1 - D)) + (9*(2 - D)*PP*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, -1 + de, 0}])/(8*(1 - D)) +
     3*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, de, -2}] -
     (3*(2 - D)*PP^2*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, de, 0}])/
      (8*(1 - D)) - ((2 + D)*gti[{a, 0, c, 0, 0},
         {al, -3 + be, -3 + ga, de, 0}])/(8*(1 - D)*PP^3) +
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {al, -3 + be, -2 + ga, de, 0}])/
      (8*(1 - D)*PP^2) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {al, -3 + be, -1 + ga, de, 0}])/(8*(1 - D)*PP) \
+ ((10 - 7*D)*gti[{a, 0, c, 0, 0}, {al, -3 + be, ga, de, 0}])/(8*(1 - D)) +
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {al, -2 + be, -3 + ga, -1 + de, 0}])/
      (8*(1 - D)*PP^3) + (3*(2 - D)*
        gti[{a, 0, c, 0, 0}, {al, -2 + be, -3 + ga, de, 0}])/
      (8*(1 - D)*PP^2) - (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {al, -2 + be, -2 + ga, -1 + de, 0}])/
      (8*(1 - D)*PP^2) - (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {al, -2 + be, -2 + ga, de, 0}])/(8*(1 - D)*PP) \
+ (9*(2 + D)*gti[{a, 0, c, 0, 0}, {al, -2 + be, -1 + ga, -1 + de, 0}])/
      (8*(1 - D)*PP) + (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {al, -2 + be, -1 + ga, de, 0}])/(8*(1 - D)) -
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {al, -2 + be, ga, -1 + de, 0}])/
      (8*(1 - D)) - 3*gti[{a, 0, c, 0, 0}, {al, -2 + be, ga, de, -1}] -
     (3*(2 - D)*PP*gti[{a, 0, c, 0, 0}, {al, -2 + be, ga, de, 0}])/
      (8*(1 - D)) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {al, -1 + be, -3 + ga, -2 + de, 0}])/
      (8*(1 - D)*PP^3) + (3*D*
        gti[{a, 0, c, 0, 0}, {al, -1 + be, -3 + ga, -1 + de, 0}])/
      (4*(1 - D)*PP^2) + (3*(2 - D)*
        gti[{a, 0, c, 0, 0}, {al, -1 + be, -3 + ga, de, 0}])/(8*(1 - D)*PP) \
+ (9*(2 + D)*gti[{a, 0, c, 0, 0}, {al, -1 + be, -2 + ga, -2 + de, 0}])/
      (8*(1 - D)*PP^2) - (9*D*
        gti[{a, 0, c, 0, 0}, {al, -1 + be, -2 + ga, -1 + de, 0}])/
      (4*(1 - D)*PP) - (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {al, -1 + be, -2 + ga, de, 0}])/(8*(1 - D)) -
     (9*(2 + D)*gti[{a, 0, c, 0, 0}, {al, -1 + be, -1 + ga, -2 + de, 0}])/
      (8*(1 - D)*PP) + (9*D*gti[{a, 0, c, 0, 0},
         {al, -1 + be, -1 + ga, -1 + de, 0}])/(4*(1 - D)) +
     (9*(2 - D)*PP*gti[{a, 0, c, 0, 0}, {al, -1 + be, -1 + ga, de, 0}])/
      (8*(1 - D)) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, -2 + de, 0}])/(8*(1 - D)) -
     (3*D*PP*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, -1 + de, 0}])/
      (4*(1 - D)) + 3*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, de, -2}] -
     (3*(2 - D)*PP^2*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, de, 0}])/
      (8*(1 - D)) + ((2 + D)*gti[{a, 0, c, 0, 0},
         {al, be, -3 + ga, -3 + de, 0}])/(8*(1 - D)*PP^3) -
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {al, be, -3 + ga, -2 + de, 0}])/
      (8*(1 - D)*PP^2) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {al, be, -3 + ga, -1 + de, 0}])/(8*(1 - D)*PP) \
- ((2 + D)*gti[{a, 0, c, 0, 0}, {al, be, -3 + ga, de, 0}])/(8*(1 - D)) -
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {al, be, -2 + ga, -3 + de, 0}])/
      (8*(1 - D)*PP^2) + (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {al, be, -2 + ga, -2 + de, 0}])/(8*(1 - D)*PP) \
- (9*(2 + D)*gti[{a, 0, c, 0, 0}, {al, be, -2 + ga, -1 + de, 0}])/
      (8*(1 - D)) + (3*(2 + D)*PP*
        gti[{a, 0, c, 0, 0}, {al, be, -2 + ga, de, 0}])/(8*(1 - D)) +
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, -3 + de, 0}])/
      (8*(1 - D)*PP) - (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, -2 + de, 0}])/(8*(1 - D)) +
     (9*(2 + D)*PP*gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, -1 + de, 0}])/
      (8*(1 - D)) - (3*(2 + D)*PP^2*
        gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, de, 0}])/(8*(1 - D)) -
     ((2 + D)*gti[{a, 0, c, 0, 0}, {al, be, ga, -3 + de, 0}])/(8*(1 - D)) +
     (3*(2 + D)*PP*gti[{a, 0, c, 0, 0}, {al, be, ga, -2 + de, 0}])/
      (8*(1 - D)) - (3*(2 + D)*PP^2*
        gti[{a, 0, c, 0, 0}, {al, be, ga, -1 + de, 0}])/(8*(1 - D)) +
     ((2 + D)*PP^3*gti[{a, 0, c, 0, 0}, {al, be, ga, de, 0}])/(8*(1 - D))
,


(*SR1*)
gti[{a_, 0, c_, 0, 0}, {al_, be_, ga_, -1, ep_}] :>
    -gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, 0, ep}]/2 +
     gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, 0, ep}]/2 +
     gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, 0, ep}]/2 +
     gti[{a, 0, c, 0, 0}, {al, be, ga, 0, -1 + ep}]/2 +
     (PP*gti[{a, 0, c, 0, 0}, {al, be, ga, 0, ep}])/2 +
     gti[{a, 0, c, 0, 0}, {1 + al, -1 + be, -1 + ga, 0, ep}]/2 -
     (PP*gti[{a, 0, c, 0, 0}, {1 + al, -1 + be, ga, 0, ep}])/2 -
     gti[{a, 0, c, 0, 0}, {1 + al, be, -1 + ga, 0, -1 + ep}]/2 +
     (PP*gti[{a, 0, c, 0, 0}, {1 + al, be, ga, 0, -1 + ep}])/2
,
(*SR2*)
gti[{a_, b_, c_, d_, e_}, {al_, be_, ga_, -1, ep_}] :>
    2*Dp*gti[{-2 + a, 1 + b, c, d, e}, {-1 + al, be, ga, 0, ep}] -
      Dp*gti[{-1 + a, b, c, d, e}, {-1 + al, be, ga, 0, ep}] -
      Dp*gti[{-1 + a, b, c, d, e}, {al, -1 + be, ga, 0, ep}] +
      Dp*gti[{-1 + a, b, c, d, e}, {al, be, ga, 0, -1 + ep}] -
      gti[{-1 + a, 1 + b, c, d, e}, {-1 + al, be, ga, 0, ep}] +
      gti[{-1 + a, 1 + b, c, d, e}, {al, be, -1 + ga, 0, ep}] -
      PP*gti[{-1 + a, 1 + b, c, d, e}, {al, be, ga, 0, ep}] +
      gti[{a, b, c, d, e}, {al, -1 + be, ga, 0, ep}] +
      PP*gti[{a, b, c, d, e}, {al, be, ga, 0, ep}] /;
     (b =!= 0) || (d =!= 0) || (e =!= 0)
,
(*SR3*)
gti[{a_, 0, c_, 0, 0}, {al_, be_, ga_, -2, ep_}] :>
    -(D*gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, 0, ep}])/(4*(1 - D)) +
     ((2 - D)*gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, ga, 0, ep}])/
      (2*(1 - D)) + (D*gti[{a, 0, c, 0, 0},
         {-1 + al, be, -1 + ga, 0, ep}])/(2*(1 - D)) +
     (D*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, 0, -1 + ep}])/(2*(1 - D)) +
     ((2 - D)*PP*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, 0, ep}])/
      (2*(1 - D)) - ((4 - 3*D)*
        gti[{a, 0, c, 0, 0}, {al, -2 + be, ga, 0, ep}])/(4*(1 - D)) -
     ((2 - D)*gti[{a, 0, c, 0, 0}, {al, -1 + be, -1 + ga, 0, ep}])/
      (1 - D) + 2*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, -1, ep}] +
     (D*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, 0, -1 + ep}])/(2*(1 - D)) -
     ((2 - D)*PP*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, 0, ep}])/(1 - D) -
     (D*gti[{a, 0, c, 0, 0}, {al, be, -2 + ga, 0, ep}])/(4*(1 - D)) -
     (D*gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, 0, -1 + ep}])/(1 - D) +
     (D*PP*gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, 0, ep}])/(2*(1 - D)) +
     2*PP*gti[{a, 0, c, 0, 0}, {al, be, ga, -1, ep}] -
     (D*gti[{a, 0, c, 0, 0}, {al, be, ga, 0, -2 + ep}])/(4*(1 - D)) -
     ((2 - D)*PP*gti[{a, 0, c, 0, 0}, {al, be, ga, 0, -1 + ep}])/(1 - D) -
     ((4 - 3*D)*PP^2*gti[{a, 0, c, 0, 0}, {al, be, ga, 0, ep}])/
      (4*(1 - D)) + (D*gti[{a, 0, c, 0, 0},
         {1 + al, -2 + be, -1 + ga, 0, ep}])/(2*(1 - D)) +
     ((2 - D)*PP*gti[{a, 0, c, 0, 0}, {1 + al, -2 + be, ga, 0, ep}])/
      (2*(1 - D)) + ((2 - D)*gti[{a, 0, c, 0, 0},
         {1 + al, -1 + be, -2 + ga, 0, ep}])/(2*(1 - D)) -
     (D*gti[{a, 0, c, 0, 0}, {1 + al, -1 + be, -1 + ga, 0, -1 + ep}])/
      (1 - D) - ((2 - D)*PP*gti[{a, 0, c, 0, 0},
         {1 + al, -1 + be, -1 + ga, 0, ep}])/(1 - D) -
     ((2 - D)*PP*gti[{a, 0, c, 0, 0}, {1 + al, -1 + be, ga, 0, -1 + ep}])/
      (1 - D) + ((2 - D)*PP^2*
        gti[{a, 0, c, 0, 0}, {1 + al, -1 + be, ga, 0, ep}])/(2*(1 - D)) +
     (D*gti[{a, 0, c, 0, 0}, {1 + al, be, -2 + ga, 0, -1 + ep}])/
      (2*(1 - D)) + (D*gti[{a, 0, c, 0, 0},
         {1 + al, be, -1 + ga, 0, -2 + ep}])/(2*(1 - D)) -
     (D*PP*gti[{a, 0, c, 0, 0}, {1 + al, be, -1 + ga, 0, -1 + ep}])/
      (1 - D) + ((2 - D)*PP*gti[{a, 0, c, 0, 0},
         {1 + al, be, ga, 0, -2 + ep}])/(2*(1 - D)) +
     (D*PP^2*gti[{a, 0, c, 0, 0}, {1 + al, be, ga, 0, -1 + ep}])/
      (2*(1 - D)) - (D*gti[{a, 0, c, 0, 0},
         {2 + al, -2 + be, -2 + ga, 0, ep}])/(4*(1 - D)) +
     (D*PP*gti[{a, 0, c, 0, 0}, {2 + al, -2 + be, -1 + ga, 0, ep}])/
      (2*(1 - D)) - (D*PP^2*gti[{a, 0, c, 0, 0},
         {2 + al, -2 + be, ga, 0, ep}])/(4*(1 - D)) +
     (D*gti[{a, 0, c, 0, 0}, {2 + al, -1 + be, -2 + ga, 0, -1 + ep}])/
      (2*(1 - D)) - (D*PP*gti[{a, 0, c, 0, 0},
         {2 + al, -1 + be, -1 + ga, 0, -1 + ep}])/(1 - D) +
     (D*PP^2*gti[{a, 0, c, 0, 0}, {2 + al, -1 + be, ga, 0, -1 + ep}])/
      (2*(1 - D)) - (D*gti[{a, 0, c, 0, 0},
         {2 + al, be, -2 + ga, 0, -2 + ep}])/(4*(1 - D)) +
     (D*PP*gti[{a, 0, c, 0, 0}, {2 + al, be, -1 + ga, 0, -2 + ep}])/
      (2*(1 - D)) - (D*PP^2*gti[{a, 0, c, 0, 0},
         {2 + al, be, ga, 0, -2 + ep}])/(4*(1 - D))
,
(*SR4*)
gti[{a_, b_, c_, d_, e_}, {al_, be_, ga_, -2, ep_}] :>
    (4*(1 - D)*Dp^2*gti[{-4 + a, 2 + b, c, d, e},
         {-2 + al, be, ga, 0, ep}])/(2 - D) -
     (4*(1 - D)*Dp^2*gti[{-3 + a, 1 + b, c, d, e},
         {-2 + al, be, ga, 0, ep}])/(2 - D) -
     (4*(1 - D)*Dp^2*gti[{-3 + a, 1 + b, c, d, e},
         {-1 + al, -1 + be, ga, 0, ep}])/(2 - D) +
     (4*(1 - D)*Dp^2*gti[{-3 + a, 1 + b, c, d, e},
         {-1 + al, be, ga, 0, -1 + ep}])/(2 - D) -
     (4*(1 - D)*Dp*gti[{-3 + a, 2 + b, c, d, e},
         {-2 + al, be, ga, 0, ep}])/(2 - D) +
     (4*(1 - D)*Dp*gti[{-3 + a, 2 + b, c, d, e},
         {-1 + al, be, -1 + ga, 0, ep}])/(2 - D) -
     (4*(1 - D)*Dp*PP*gti[{-3 + a, 2 + b, c, d, e},
         {-1 + al, be, ga, 0, ep}])/(2 - D) +
     Dp^2*gti[{-2 + a, b, c, d, e}, {-2 + al, be, ga, 0, ep}] -
     (2*D*Dp^2*gti[{-2 + a, b, c, d, e}, {-1 + al, -1 + be, ga, 0, ep}])/
      (2 - D) - 2*Dp^2*gti[{-2 + a, b, c, d, e},
       {-1 + al, be, ga, 0, -1 + ep}] +
     Dp^2*gti[{-2 + a, b, c, d, e}, {al, -2 + be, ga, 0, ep}] -
     2*Dp^2*gti[{-2 + a, b, c, d, e}, {al, -1 + be, ga, 0, -1 + ep}] +
     Dp^2*gti[{-2 + a, b, c, d, e}, {al, be, ga, 0, -2 + ep}] -
     (2*D*Dp*gti[{-2 + a, 1 + b, c, d, e}, {-2 + al, be, ga, 0, ep}])/
      (2 - D) - (2*D*Dp*gti[{-2 + a, 1 + b, c, d, e},
         {-1 + al, -1 + be, ga, 0, ep}])/(2 - D) +
     (2*D*Dp*gti[{-2 + a, 1 + b, c, d, e}, {-1 + al, be, -1 + ga, 0, ep}])/
      (2 - D) + (2*D*Dp*gti[{-2 + a, 1 + b, c, d, e},
         {-1 + al, be, ga, 0, -1 + ep}])/(2 - D) -
     (2*D*Dp*PP*gti[{-2 + a, 1 + b, c, d, e}, {-1 + al, be, ga, 0, ep}])/
      (2 - D) + (2*D*Dp*gti[{-2 + a, 1 + b, c, d, e},
         {al, -1 + be, -1 + ga, 0, ep}])/(2 - D) -
     (2*D*Dp*PP*gti[{-2 + a, 1 + b, c, d, e}, {al, -1 + be, ga, 0, ep}])/
      (2 - D) - (2*D*Dp*gti[{-2 + a, 1 + b, c, d, e},
         {al, be, -1 + ga, 0, -1 + ep}])/(2 - D) +
     (2*D*Dp*PP*gti[{-2 + a, 1 + b, c, d, e}, {al, be, ga, 0, -1 + ep}])/
      (2 - D) + gti[{-2 + a, 2 + b, c, d, e}, {-2 + al, be, ga, 0, ep}] -
     2*gti[{-2 + a, 2 + b, c, d, e}, {-1 + al, be, -1 + ga, 0, ep}] -
     (2*D*PP*gti[{-2 + a, 2 + b, c, d, e}, {-1 + al, be, ga, 0, ep}])/
      (2 - D) + gti[{-2 + a, 2 + b, c, d, e}, {al, be, -2 + ga, 0, ep}] -
     2*PP*gti[{-2 + a, 2 + b, c, d, e}, {al, be, -1 + ga, 0, ep}] +
     PP^2*gti[{-2 + a, 2 + b, c, d, e}, {al, be, ga, 0, ep}] +
     (4*Dp*gti[{-1 + a, b, c, d, e}, {-1 + al, -1 + be, ga, 0, ep}])/
      (2 - D) - (4*Dp*gti[{-1 + a, b, c, d, e},
         {al, -1 + be, -1 + ga, 0, ep}])/(2 - D) +
     (4*Dp*PP*gti[{-1 + a, b, c, d, e}, {al, -1 + be, ga, 0, ep}])/
      (2 - D) + (4*PP*gti[{-1 + a, 1 + b, c, d, e},
         {-1 + al, be, ga, 0, ep}])/(2 - D) +
     (4*PP*gti[{-1 + a, 1 + b, c, d, e}, {al, -1 + be, ga, 0, ep}])/
      (2 - D) - (4*PP*gti[{-1 + a, 1 + b, c, d, e},
         {al, be, ga, 0, -1 + ep}])/(2 - D) -
     gti[{a, b, c, d, e}, {al, -2 + be, ga, 0, ep}] +
     2*gti[{a, b, c, d, e}, {al, -1 + be, ga, -1, ep}] -
     (2*(4 - D)*PP*gti[{a, b, c, d, e}, {al, -1 + be, ga, 0, ep}])/
      (2 - D) + 2*PP*gti[{a, b, c, d, e}, {al, be, ga, -1, ep}] -
     PP^2*gti[{a, b, c, d, e}, {al, be, ga, 0, ep}] /;
     (b =!= 0) || (d =!= 0) || (e =!= 0)
,
(*SR5*)
gti[{a_, 0, c_, 0, 0}, {al_, be_, ga_, -3, ep_}] :>
    ((2 + D)*gti[{a, 0, c, 0, 0}, {-3 + al, be, ga, 0, ep}])/(8*(1 - D)) -
     (3*(2 - D)*gti[{a, 0, c, 0, 0}, {-2 + al, -1 + be, ga, 0, ep}])/
      (8*(1 - D)) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-2 + al, be, -1 + ga, 0, ep}])/(8*(1 - D)) -
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, 0, -1 + ep}])/
      (8*(1 - D)) - (3*(2 - D)*PP*
        gti[{a, 0, c, 0, 0}, {-2 + al, be, ga, 0, ep}])/(8*(1 - D)) -
     (3*(2 - D)*gti[{a, 0, c, 0, 0}, {-1 + al, -2 + be, ga, 0, ep}])/
      (8*(1 - D)) + (9*(2 - D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, -1 + ga, 0, ep}])/
      (8*(1 - D)) - (3*D*gti[{a, 0, c, 0, 0},
         {-1 + al, -1 + be, ga, 0, -1 + ep}])/(4*(1 - D)) -
     (3*(2 - 3*D)*PP*gti[{a, 0, c, 0, 0}, {-1 + al, -1 + be, ga, 0, ep}])/
      (8*(1 - D)) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, -2 + ga, 0, ep}])/(8*(1 - D)) +
     (9*(2 + D)*gti[{a, 0, c, 0, 0}, {-1 + al, be, -1 + ga, 0, -1 + ep}])/
      (8*(1 - D)) - (3*D*PP*gti[{a, 0, c, 0, 0},
         {-1 + al, be, -1 + ga, 0, ep}])/(4*(1 - D)) +
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, 0, -2 + ep}])/
      (8*(1 - D)) + (9*(2 - D)*PP*
        gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, 0, -1 + ep}])/(8*(1 - D)) -
     (3*(2 - D)*PP^2*gti[{a, 0, c, 0, 0}, {-1 + al, be, ga, 0, ep}])/
      (8*(1 - D)) + ((10 - 7*D)*
        gti[{a, 0, c, 0, 0}, {al, -3 + be, ga, 0, ep}])/(8*(1 - D)) +
     (9*(2 - D)*gti[{a, 0, c, 0, 0}, {al, -2 + be, -1 + ga, 0, ep}])/
      (8*(1 - D)) - 3*gti[{a, 0, c, 0, 0}, {al, -2 + be, ga, -1, ep}] -
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {al, -2 + be, ga, 0, -1 + ep}])/
      (8*(1 - D)) + (3*(6 - 5*D)*PP*
        gti[{a, 0, c, 0, 0}, {al, -2 + be, ga, 0, ep}])/(8*(1 - D)) -
     (9*(2 - D)*gti[{a, 0, c, 0, 0}, {al, -1 + be, -2 + ga, 0, ep}])/
      (8*(1 - D)) + (9*D*gti[{a, 0, c, 0, 0},
         {al, -1 + be, -1 + ga, 0, -1 + ep}])/(4*(1 - D)) +
     (3*(4 - 3*D)*PP*gti[{a, 0, c, 0, 0}, {al, -1 + be, -1 + ga, 0, ep}])/
      (4*(1 - D)) + 3*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, -2, ep}] -
     6*PP*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, -1, ep}] +
     (3*(2 + D)*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, 0, -2 + ep}])/
      (8*(1 - D)) + (3*(4 - 3*D)*PP*
        gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, 0, -1 + ep}])/(4*(1 - D)) +
     (3*(6 - 5*D)*PP^2*gti[{a, 0, c, 0, 0}, {al, -1 + be, ga, 0, ep}])/
      (8*(1 - D)) - ((2 + D)*gti[{a, 0, c, 0, 0},
         {al, be, -3 + ga, 0, ep}])/(8*(1 - D)) -
     (9*(2 + D)*gti[{a, 0, c, 0, 0}, {al, be, -2 + ga, 0, -1 + ep}])/
      (8*(1 - D)) + (3*(2 + D)*PP*
        gti[{a, 0, c, 0, 0}, {al, be, -2 + ga, 0, ep}])/(8*(1 - D)) -
     (9*(2 + D)*gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, 0, -2 + ep}])/
      (8*(1 - D)) + (9*D*PP*gti[{a, 0, c, 0, 0},
         {al, be, -1 + ga, 0, -1 + ep}])/(4*(1 - D)) -
     (3*(2 + D)*PP^2*gti[{a, 0, c, 0, 0}, {al, be, -1 + ga, 0, ep}])/
      (8*(1 - D)) + 3*PP*gti[{a, 0, c, 0, 0}, {al, be, ga, -2, ep}] -
     3*PP^2*gti[{a, 0, c, 0, 0}, {al, be, ga, -1, ep}] -
     ((2 + D)*gti[{a, 0, c, 0, 0}, {al, be, ga, 0, -3 + ep}])/(8*(1 - D)) -
     (9*(2 - D)*PP*gti[{a, 0, c, 0, 0}, {al, be, ga, 0, -2 + ep}])/
      (8*(1 - D)) + (9*(2 - D)*PP^2*
        gti[{a, 0, c, 0, 0}, {al, be, ga, 0, -1 + ep}])/(8*(1 - D)) +
     ((10 - 7*D)*PP^3*gti[{a, 0, c, 0, 0}, {al, be, ga, 0, ep}])/
      (8*(1 - D)) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {1 + al, -3 + be, -1 + ga, 0, ep}])/
      (8*(1 - D)) - (3*(2 - D)*PP*
        gti[{a, 0, c, 0, 0}, {1 + al, -3 + be, ga, 0, ep}])/(8*(1 - D)) -
     (9*(2 - D)*gti[{a, 0, c, 0, 0}, {1 + al, -2 + be, -2 + ga, 0, ep}])/
      (8*(1 - D)) + (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {1 + al, -2 + be, -1 + ga, 0, -1 + ep}])/
      (8*(1 - D)) + (3*(4 - 3*D)*PP*
        gti[{a, 0, c, 0, 0}, {1 + al, -2 + be, -1 + ga, 0, ep}])/
      (4*(1 - D)) + (9*(2 - D)*PP*
        gti[{a, 0, c, 0, 0}, {1 + al, -2 + be, ga, 0, -1 + ep}])/
      (8*(1 - D)) - (3*(2 - 3*D)*PP^2*
        gti[{a, 0, c, 0, 0}, {1 + al, -2 + be, ga, 0, ep}])/(8*(1 - D)) +
     (3*(2 - D)*gti[{a, 0, c, 0, 0}, {1 + al, -1 + be, -3 + ga, 0, ep}])/
      (8*(1 - D)) - (9*D*gti[{a, 0, c, 0, 0},
         {1 + al, -1 + be, -2 + ga, 0, -1 + ep}])/(4*(1 - D)) -
     (9*(2 - D)*PP*gti[{a, 0, c, 0, 0},
         {1 + al, -1 + be, -2 + ga, 0, ep}])/(8*(1 - D)) -
     (9*(2 + D)*gti[{a, 0, c, 0, 0},
         {1 + al, -1 + be, -1 + ga, 0, -2 + ep}])/(8*(1 - D)) -
     (3*(2 - 3*D)*PP*gti[{a, 0, c, 0, 0},
         {1 + al, -1 + be, -1 + ga, 0, -1 + ep}])/(2*(1 - D)) +
     (9*(2 - D)*PP^2*gti[{a, 0, c, 0, 0},
         {1 + al, -1 + be, -1 + ga, 0, ep}])/(8*(1 - D)) -
     (9*(2 - D)*PP*gti[{a, 0, c, 0, 0},
         {1 + al, -1 + be, ga, 0, -2 + ep}])/(8*(1 - D)) +
     (3*(4 - 3*D)*PP^2*gti[{a, 0, c, 0, 0},
         {1 + al, -1 + be, ga, 0, -1 + ep}])/(4*(1 - D)) -
     (3*(2 - D)*PP^3*gti[{a, 0, c, 0, 0}, {1 + al, -1 + be, ga, 0, ep}])/
      (8*(1 - D)) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {1 + al, be, -3 + ga, 0, -1 + ep}])/
      (8*(1 - D)) + (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {1 + al, be, -2 + ga, 0, -2 + ep}])/
      (8*(1 - D)) - (9*(2 + D)*PP*
        gti[{a, 0, c, 0, 0}, {1 + al, be, -2 + ga, 0, -1 + ep}])/
      (8*(1 - D)) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {1 + al, be, -1 + ga, 0, -3 + ep}])/
      (8*(1 - D)) - (9*D*PP*gti[{a, 0, c, 0, 0},
         {1 + al, be, -1 + ga, 0, -2 + ep}])/(4*(1 - D)) +
     (9*(2 + D)*PP^2*gti[{a, 0, c, 0, 0},
         {1 + al, be, -1 + ga, 0, -1 + ep}])/(8*(1 - D)) +
     (3*(2 - D)*PP*gti[{a, 0, c, 0, 0}, {1 + al, be, ga, 0, -3 + ep}])/
      (8*(1 - D)) - (9*(2 - D)*PP^2*
        gti[{a, 0, c, 0, 0}, {1 + al, be, ga, 0, -2 + ep}])/(8*(1 - D)) -
     (3*(2 + D)*PP^3*gti[{a, 0, c, 0, 0}, {1 + al, be, ga, 0, -1 + ep}])/
      (8*(1 - D)) + (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {2 + al, -3 + be, -2 + ga, 0, ep}])/
      (8*(1 - D)) - (3*D*PP*gti[{a, 0, c, 0, 0},
         {2 + al, -3 + be, -1 + ga, 0, ep}])/(4*(1 - D)) -
     (3*(2 - D)*PP^2*gti[{a, 0, c, 0, 0}, {2 + al, -3 + be, ga, 0, ep}])/
      (8*(1 - D)) + (3*(2 - D)*
        gti[{a, 0, c, 0, 0}, {2 + al, -2 + be, -3 + ga, 0, ep}])/
      (8*(1 - D)) - (9*(2 + D)*
        gti[{a, 0, c, 0, 0}, {2 + al, -2 + be, -2 + ga, 0, -1 + ep}])/
      (8*(1 - D)) - (9*(2 - D)*PP*
        gti[{a, 0, c, 0, 0}, {2 + al, -2 + be, -2 + ga, 0, ep}])/
      (8*(1 - D)) + (9*D*PP*gti[{a, 0, c, 0, 0},
         {2 + al, -2 + be, -1 + ga, 0, -1 + ep}])/(4*(1 - D)) +
     (9*(2 - D)*PP^2*gti[{a, 0, c, 0, 0},
         {2 + al, -2 + be, -1 + ga, 0, ep}])/(8*(1 - D)) +
     (9*(2 - D)*PP^2*gti[{a, 0, c, 0, 0},
         {2 + al, -2 + be, ga, 0, -1 + ep}])/(8*(1 - D)) -
     (3*(2 - D)*PP^3*gti[{a, 0, c, 0, 0}, {2 + al, -2 + be, ga, 0, ep}])/
      (8*(1 - D)) + (3*D*gti[{a, 0, c, 0, 0},
         {2 + al, -1 + be, -3 + ga, 0, -1 + ep}])/(4*(1 - D)) +
     (9*(2 + D)*gti[{a, 0, c, 0, 0},
         {2 + al, -1 + be, -2 + ga, 0, -2 + ep}])/(8*(1 - D)) -
     (9*D*PP*gti[{a, 0, c, 0, 0}, {2 + al, -1 + be, -2 + ga, 0, -1 + ep}])/
      (4*(1 - D)) - (9*D*PP*gti[{a, 0, c, 0, 0},
         {2 + al, -1 + be, -1 + ga, 0, -2 + ep}])/(4*(1 - D)) +
     (9*D*PP^2*gti[{a, 0, c, 0, 0},
         {2 + al, -1 + be, -1 + ga, 0, -1 + ep}])/(4*(1 - D)) -
     (9*(2 - D)*PP^2*gti[{a, 0, c, 0, 0},
         {2 + al, -1 + be, ga, 0, -2 + ep}])/(8*(1 - D)) -
     (3*D*PP^3*gti[{a, 0, c, 0, 0}, {2 + al, -1 + be, ga, 0, -1 + ep}])/
      (4*(1 - D)) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {2 + al, be, -3 + ga, 0, -2 + ep}])/
      (8*(1 - D)) - (3*(2 + D)*
        gti[{a, 0, c, 0, 0}, {2 + al, be, -2 + ga, 0, -3 + ep}])/
      (8*(1 - D)) + (9*(2 + D)*PP*
        gti[{a, 0, c, 0, 0}, {2 + al, be, -2 + ga, 0, -2 + ep}])/
      (8*(1 - D)) + (3*D*PP*gti[{a, 0, c, 0, 0},
         {2 + al, be, -1 + ga, 0, -3 + ep}])/(4*(1 - D)) -
     (9*(2 + D)*PP^2*gti[{a, 0, c, 0, 0},
         {2 + al, be, -1 + ga, 0, -2 + ep}])/(8*(1 - D)) +
     (3*(2 - D)*PP^2*gti[{a, 0, c, 0, 0}, {2 + al, be, ga, 0, -3 + ep}])/
      (8*(1 - D)) + (3*(2 + D)*PP^3*
        gti[{a, 0, c, 0, 0}, {2 + al, be, ga, 0, -2 + ep}])/(8*(1 - D)) -
     ((2 + D)*gti[{a, 0, c, 0, 0}, {3 + al, -3 + be, -3 + ga, 0, ep}])/
      (8*(1 - D)) + (3*(2 + D)*PP*
        gti[{a, 0, c, 0, 0}, {3 + al, -3 + be, -2 + ga, 0, ep}])/
      (8*(1 - D)) - (3*(2 + D)*PP^2*
        gti[{a, 0, c, 0, 0}, {3 + al, -3 + be, -1 + ga, 0, ep}])/
      (8*(1 - D)) + ((2 + D)*PP^3*
        gti[{a, 0, c, 0, 0}, {3 + al, -3 + be, ga, 0, ep}])/(8*(1 - D)) +
     (3*(2 + D)*gti[{a, 0, c, 0, 0},
         {3 + al, -2 + be, -3 + ga, 0, -1 + ep}])/(8*(1 - D)) -
     (9*(2 + D)*PP*gti[{a, 0, c, 0, 0},
         {3 + al, -2 + be, -2 + ga, 0, -1 + ep}])/(8*(1 - D)) +
     (9*(2 + D)*PP^2*gti[{a, 0, c, 0, 0},
         {3 + al, -2 + be, -1 + ga, 0, -1 + ep}])/(8*(1 - D)) -
     (3*(2 + D)*PP^3*gti[{a, 0, c, 0, 0},
         {3 + al, -2 + be, ga, 0, -1 + ep}])/(8*(1 - D)) -
     (3*(2 + D)*gti[{a, 0, c, 0, 0},
         {3 + al, -1 + be, -3 + ga, 0, -2 + ep}])/(8*(1 - D)) +
     (9*(2 + D)*PP*gti[{a, 0, c, 0, 0},
         {3 + al, -1 + be, -2 + ga, 0, -2 + ep}])/(8*(1 - D)) -
     (9*(2 + D)*PP^2*gti[{a, 0, c, 0, 0},
         {3 + al, -1 + be, -1 + ga, 0, -2 + ep}])/(8*(1 - D)) +
     (3*(2 + D)*PP^3*gti[{a, 0, c, 0, 0},
         {3 + al, -1 + be, ga, 0, -2 + ep}])/(8*(1 - D)) +
     ((2 + D)*gti[{a, 0, c, 0, 0}, {3 + al, be, -3 + ga, 0, -3 + ep}])/
      (8*(1 - D)) - (3*(2 + D)*PP*
        gti[{a, 0, c, 0, 0}, {3 + al, be, -2 + ga, 0, -3 + ep}])/
      (8*(1 - D)) + (3*(2 + D)*PP^2*
        gti[{a, 0, c, 0, 0}, {3 + al, be, -1 + ga, 0, -3 + ep}])/
      (8*(1 - D)) - ((2 + D)*PP^3*
        gti[{a, 0, c, 0, 0}, {3 + al, be, ga, 0, -3 + ep}])/(8*(1 - D))
,
(* MISC *)

(*
gti[{a_/;a>=0,b_,c_,d_,e_},{0,be_,ga_,0,ep_}] :>
   gti[{d,c,b,a,e},{0,be,ga,0,ep}] /; !OrderedQ[{{d,c,b,a,e},{a,b,c,d,e}} ]
,
*)

gti[{a_,b_Integer?Positive,c_,d_,e_/;Head[e]=!=Integer},
    {dd__}
   ] :> - gti[{a, b-1,c,d,e+1}, {dd}] + gti[{a+1,b-1,c,d,e}, {dd}]
,

(*
gti[{a_Integer?Positive,b_,c_,d_,e_/;Head[e]=!=Integer},
    {dd__}
   ] :>   gti[{a-1,b,c,d,e+1}, {dd}] + gti[{a-1,b+1,c,d,e},    {dd}]
,
*)

gti[{0,-1,0,0,e_/;Head[e]=!=Integer},
    {1,1,1,1,1}
   ] :> PowerSimplify[(-1)^e] gti[{-1,0,0,0,e},{1,1,1,1,1}]
,

gti[{a_,b_,c_,d_,e_Integer?Positive},{bla__}] :>
Sum[Binomial[e,ii] (-1)^(e-ii) gti[{a+ii, b+e-ii,c,d,0}, {bla}
                                  ], {ii,0,e}]//eExpand
,
gti[{a_,b_,1,d_,e_},{bla__}] :>
 Dp gti[{a,b,0,d,e},{bla}] - gti[{a+1,b,0,d,e},{bla}]
,

gti[{a_,b_,c_,1,e_},{bla__}] :>
 Dp gti[{a,b,c,0,e},{bla}] - gti[{a,b+1,c,0,e},{bla}]
,

gti[{a_,b_Integer?Positive,c_,d_/;Head[d]=!=Integer,e_},{bla__}] :>
 Dp gti[{a,b-1,c,d,e},{bla}] - gti[{a,b-1,c,d+1,e},{bla}]
,

gti[{a_Integer?Positive,b_,c_/;Head[c]=!=Integer,d_,e_},{bla__}] :>
 Dp gti[{a-1,b,c,d,e},{bla}] - gti[{a-1,b,c+1,d,e},{bla}]
,

gti[{a_,b_,c_Integer?Positive,d_,e_},{bla__}] :>
Sum[Binomial[c,ii] (-1)^(c-ii) Dp^ii *
    gti[{a+c-ii,b,0,d,e}, {bla}], {ii,0,c}] // eExpand
,

gti[{a_,b_,c_,d_Integer?Positive,e_},{bla__}] :>
Sum[Binomial[d,ii] (-1)^(d-ii) Dp^ii *
    gti[{a,b+d-ii,c,0,e}, {bla}], {ii,0,d}] // eExpand
};

(*
gtirules = MapAt[HoldPattern, gtir, Array[{#,1}&, Length[gtir]]];
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SimplifyGTI | \n "]];
Null
