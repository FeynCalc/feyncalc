(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TLI*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 28 August '98 at 20:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  eq (3C.19) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`TLI`",{"HighEnergyPhysics`FeynCalc`"}];

TLI::"usage"=
"TLI[{v,w,x,y,z},{a,b,c,d,e}, {al,be,ga,de,ep}].
The exponents of the numerator scalar product are (dl = OPEDelta):
v: k1.k1, w: k2.k2,  x: p.k1, y: p.k2, z: k1.k2.
a: dl.k1, b: dl.k2,  c: dl.(p-k1), d: dl.(p-k2), e: dl.(k1-k2)\n\n

TLI[{mu1, ...}, {nu1, ...}][{v,w,x,y,z},{a,b,c,d,e}, {al,be,ga,de,ep}]
is a tensor integral, where mu1 belongs to k1 and nu1 to k2.\n\n

TLI[any___,{a,b,c,d,e,0,0}, {al,be,ga,de,ep}] simplifies to
TLI[any, {a,b,c,d,e}, {al,be,ga,de,ep}];\n\n

TLI[{0,0,0,0,0},{a,b,c,d,e}, {al,be,ga,de,ep}] simplifies to
TLI[{a,b,c,d,e}, {al,be,ga,de,ep}].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


Collect2      = MakeContext["Collect2"];
DeltaFunction = MakeContext["CoreObjects","DeltaFunction"];
Epsilon       = MakeContext["CoreObjects","Epsilon"];
EpsilonOrder  = MakeContext["CoreOptions","EpsilonOrder"];
Factor1       = MakeContext["Factor1"];
Factoring     = MakeContext["CoreOptions","Factoring"];
FeynCalcInternal = MakeContext["FeynCalcInternal"];
FreeQ2        = MakeContext["FreeQ2"];
Momentum      = MakeContext["CoreObjects","Momentum"];
OPEDelta      = MakeContext["OPEDelta"];
OPEm          = MakeContext["OPEm"];
PowerSimplify = MakeContext["PowerSimplify"];
SOD            = MakeContext["CoreObjects","SOD"];
ScalarProduct = MakeContext["ScalarProduct"];
SelectFree       = MakeContext["SelectFree"];
SelectNotFree       = MakeContext["SelectNotFree"];

Options[TLI] =  {EpsilonOrder -> 0, Momentum -> Global`p};



TLI[a__, pr_ /; !FreeQ[pr, {_,0}], opt___Rule] := TLI[a, pr /. {b_,0} :> b];

(*NEW 08 97 *)

    TLI[{0, 1, 0, 0, m_/;Head[m]=!=Integer}, {xyz__}, opt___Rule] :=
    -TLI[{0,0,0,0,m+1}, {xyz}, opt] + TLI[{1,0,0,0,m}, {xyz}, opt];

    TLI[{1, 0, 0, 0, m_/;Head[m]=!=Integer},
        {a_, b_, b_, a_, c_}, oot___Rule] :=
    1/2 TLI[{0, 0, 0, 0, m}, {a, b, b, a, c}] sod[oot] +
    1/2 TLI[{0, 0, 0, 0, m+1}, {a, b, b, a, c}];

TLI[{m_, 1, 0, 0, 0}, {{2, M_}, 0, 0, {1, M_}, {1, M_}}, opt___Rule] :=
(sod[opt]*TLI[{0, m, 0, 0, 0}, {{1, M}, {2, M}, 0, 1, 0}])/2 -
TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {2, M}, 0, 1, 0}]/2 +
   (sod[opt]*TLI[{m, 0, 0, 0, 0}, {{2, M}, 0, 0, {1, M}, {1, M}}])/2 -
   (sod[opt]*TLI[{m, 0, 0, 0, 0}, {{2, M}, 0, 1, {1, M}, 0}])/2 +
TLI[{1 + m, 0, 0, 0, 0}, {{2, M}, 0, 0, {1, M}, {1, M}}]/2 +
   TLI[{1 + m, 0, 0, 0, 0}, {{2, M}, 0, 1, {1, M}, 0}]/2;

(*
TLI[{m_, 1, 0, 0, 0}, {{2, M_}, 0, 1, {1, M_}, {1, M_}}, opt___Rule] :=
 -(sod[opt]*TLI[{0, m, 0, 0, 0}, {{1, M}, {2, M}, 0, 2, 0}])/2 +
   TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {2, M}, 0, 2, 0}]/2  +
  (sod[opt]*TLI[{m, 0, 0, 0, 0}, {{2, M}, 0, 1, {1, M}, {1, M}}])/2 +
   (sod[opt]*TLI[{m, 0, 0, 0, 0}, {{2, M}, 0, 2, {1, M}, 0}])/2 +
   TLI[{1 + m, 0, 0, 0, 0}, {{2, M}, 0, 1, {1, M}, {1, M}}]/2 -
   TLI[{1 + m, 0, 0, 0, 0}, {{2, M}, 0, 2, {1, M}, 0}]/2;
*)

(* true 0's *)

TLI[{a_,b_,0,d_,0}, {n1_Integer, nm2_,0,nm4_,0}, ___Rule] := 0;

(*NEW0997*)
TLI[{a_,b_,c_,0,0}, {nm1_, 0, nm3_, n4_Integer,0}, ___Rule] := 0;

TLI[{a_ /; !MatchQ[a, _Integer?Negative],
     b_  /; !MatchQ[b, _Integer?Negative],0,0,0},
    {{n1_,m1_},{n2_,m2_}, 0,0,0}, ___Rule] := 0 /; (a + b) > 0;

(* this assumes that m is generic; thus: don't use this for
m=0 *)

TLI[{a_, b_, 0,0,e_}, {x_, y_, 0, 0, z_}, opt___Rule] := 0 /;
 (Head[a]=!=Integer) ||  (Head[b]=!=Integer) || (Head[e]=!=Integer);

TLI[{0,0,a_, b_, e_}, {0, 0, x_, y_, z_}, opt___Rule] := 0 /;
 (Head[a]=!=Integer) ||  (Head[b]=!=Integer) || (Head[e]=!=Integer);

(* end true 0's *)

(* special cases ; all checked by TARCER *)

TLI[{0, 0, 0, 1, 0}, {m_, 0, 0, 0, 0}, {{2, M_}, {1, M_}, 1, 0, 1},
opt___Rule] := -TLI[{m, 0, 0, 0, 0}, {1, 0, 1, 0, {1, M}}]/(4*M^2) +
   TLI[{m, 0, 0, 0, 0}, {1, 1, 1, 0, {1, M}}]/4 +
   TLI[{m, 0, 0, 0, 0}, {{1, M}, 0, 1, 0, {1, M}}]/(4*M^2) -
   TLI[{m, 0, 0, 0, 0}, {{2, M}, 0, 1, 0, {1, M}}]/2 +
   TLI[{m, 0, 0, 0, 0}, {{2, M}, 1, 1, 0, 0}]/4 +
   M^2*TLI[{m, 0, 0, 0, 0}, {{2, M}, 1, 1, 0, {1, M}}];

 TLI[{1, m_, 0, 0, 0}, {{1, M_}, {1, M_}, 0, 1, 1}, opt___Rule]  :=
   TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, 1, 0, 1, 0}]/(2*M^2) -
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, 1, 0, 1, 1}]/2 -
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {1, M}, 0, 1, 0}]/(2*M^2) +
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {1, M}, 0, 1, 1}] ;

  TLI[{1, m_, 0, 0, 0}, {{1, M_}, {2, M_}, 0, 0, 1}, opt___Rule] :=
   TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, 1, 0, 0, 1}]/(2*M^2) -
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {1, M}, 0, 0, 1}]/(2*M^2) -
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {2, M}, 0, 0, 0}]/(2*M^2) +
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {2, M}, 0, 0, 1}];

  TLI[{1, m_, 0, 0, 0}, {{1, M_}, {2, M_}, 0, 1, 1}, opt___Rule] :=
  -TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, 1, 0, 1, 0}]/(2*M^4) +
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, 1, 0, 1, 1}]/(2*M^2) +
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {1, M}, 0, 1, 0}]/(2*M^4) -
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {1, M}, 0, 1, 1}]/(2*M^2) -
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {2, M}, 0, 1, 0}]/(2*M^2) +
    TLI[{0, 1 + m, 0, 0, 0}, {{1, M}, {2, M}, 0, 1, 1}];

  TLI[{m_, 1, 0, 0, 0}, {{2, M_}, {1, M_}, 0, 1, 0}, opt___Rule] :=
   -(sod[opt]*TLI[{0, m, 0, 0, 0}, {{1, M}, {2, M}, 0, 0, 0}])/(2*M^2) +
    sod[opt]*TLI[{m, 0, 0, 0, 0}, {{2, M}, {1, M}, 0, 1, 0}];


TLI[xx___,{a_,b_,c_,d_,e_/;e>0},li_List, opt___Rule] :=
 TLI[xx,{a+1,b,c,d,e-1},li,opt] - TLI[xx,{a,b+1,c,d,e-1},li,opt] ;

TLI[xx___,{a_,b_,c_,d_/;d>0,e_},li_List, opt___Rule] :=
 sod[opt] TLI[xx,{a,b,c,d-1,e},li,opt] - TLI[xx,{a,b+1,c,d-1,e},li,opt] ;

TLI[xx___,{a_,b_,c_/;c>0,d_,e_},li_List, opt___Rule] :=
 sod[opt] TLI[xx,{a,b,c-1,d,e},li,opt] - TLI[xx,{a+1,b,c-1,d,e},li,opt] ;


TLI[{1,0,0,0,0}, {a__},{0,b__}, opt___Rule] := TLI[{a},{-1,b},opt];
TLI[{2,0,0,0,0}, {a__},{0,b__}, opt___Rule] := TLI[{a},{-2,b},opt];

TLI[{0,1,0,0,0}, {a__},{b_,0,c__}, opt___Rule] := TLI[{a},{b,-1,c},opt];
TLI[{0,2,0,0,0}, {a__},{b_,0,c__}, opt___Rule] := TLI[{a},{b,-2,c},opt];


(*DANGER? commented out August 2nd 1997
TLI[{m_ /; Head[m]=!=Integer, g2_Integer?Positive, g3_, g4_, 0},
     {{a1_, M_Symbol}, {a2_, M_Symbol}, a3_, 0, 0}, r___
   ] := 0;
*)

TLI[{m_ /; Head[m]=!=Integer, g2_Integer, g3_Integer,
     g4_Integer, g5_Integer
    }, {{a1_, M_Symbol}, a2_, a3_, 0, {a5_, M_Symbol}}
   ] := TLI[{m, g5, g3, g4, g2}, {{a1, M}, {a5, M}, a3, 0, a2}];

(*RM; Dec.28 *)
TLI[{a_Integer,b_Integer,c_/;Head[c]=!=Integer,d_Integer,e_Integer},
    {al_,be_,ga_,de_,ep_}, rul___Rule] :=
 (-1)^e TLI[{c,d,a,b,e},{ga,de,al,be,ep}, rul];

(*Rainer; Dec.28 *)
TLI[{0,b_,0,d_,0}, {-1,be_Integer?Positive,0,de_,{1,MI_}}] :=
    TLI[{0,b,0,d,0}, {0,be-1,0,de,{1,MI}}] +
 MI^2 TLI[{0,b,0,d,0}, {0,be,0,de,{1,MI}}];

(*RM; Dec.21 *)
(* move the m from the 5th position to the second: *)
TLI[{a_,b_Integer,c_,0,e_/;Head[e]=!=Integer},
    {al_,be_,ga_,0,ep_}, rul___Rule] :=
TLI[{a,e,c,0,b}, {al,ep,ga,0,be}, rul];

(*Rainer; Aug.26/97*)
TLI[{g5_Integer, m_ /; Head[m]=!=Integer, 0, g4_Integer, g1_Integer},
    {a5_, a2_, 0, a4_, {a1_, M_Symbol}} ] :=
(-1)^(g1+g5) * TLI[{g1, m, 0, g4, g5}, {{a1, M}, a2, 0, a4, a5}];

(* NEW relation for graph11 *)
TLI[{a1_Integer, a2_Integer, a3_Integer, m_/;Head[m]=!=Integer, 0},
    {g1_,g2_,g3_,g4_,g5_}, ___Rule
   ] := TLI[{m,a3,a2,a1,0}, {g4,g3,g2,g1,g5}];

TLI[{a1_Integer, a2_Integer, a3_Integer, m_, 0},
    {g1_Integer,g2_,g3_, {g4_,M_Symbol}, g5_}, ___Rule
   ] := TLI[{m,a3,a2,a1,0}, {{g4, M},g3,g2,g1,g5}];

(* k1 <--> k2 *)
TLI[{a_Integer,b_ /; Head[b]=!=Integer,c_,d_,e_},
    {al_,be_,ga_/;ga=!=0,0,ep_},
    opt___] := (-1)^e TLI[{b,a,d,c,e},{be,al,0,ga,ep},opt];

(* k1 <--> k2 *)
TLI[{a_ /; Head[a]=!=Integer,b_Integer, c_,d_,e_},{al_,be_,ga_,0,ep_},
    opt___] := (-1)^e TLI[{b,a,d,c,e},{be,al,0,ga,ep},opt];

TLI[{0,0,0,0,0},{x__},{y__}, opt___Rule] := TLI[{x}, {y}, opt];
TLI[{x1_,x2_,x3_,x4_,x5_, 0, 0}, {y__}] :=  TLI[{x1,x2,x3,x4,x5},{y}];

(* NEW relation for graph13 (02/98); however, dangerious for graph17 ... *)
TLI[{a1_, a2_Integer?Positive, a3_, a4_, m_/;Head[m]=!=Integer},
    {g1_,g2_,g3_,g4_,g5_}, ___Rule
   ] :=( TLI[{a1+1,a2-1,a3,a4,m}, {g1,g2,g3,g4,g5}] -
        TLI[{a1,a2-1,a3,a4,m+1}, {g1,g2,g3,g4,g5}] ) /;
  (Global`$SpecialTLI) =!= False;


(* ************************************************************* *)

(* this generates Delta.p *)
sod[rul___Rule] := FeynCalcInternal[SOD[Momentum /. {rul} /.
                      Options[TLI]]];
   ma[i_]:= StyleBox[i//ToString,
                            FontColor -> RGBColor[0,0,1]
                           ];

   ma[{i_, m_}] := StyleBox[ToString[i],
                                   FontWeight -> "Bold",
                                   FontColor -> RGBColor[1,0,0]
                                  ];
   TLI /:
   MakeBoxes[TLI[{a__}, {al__}], TraditionalForm] :=
     SubsuperscriptBox[
 StyleBox["T", FontFamily -> "Times",
               FontWeight -> "Bold"
         ],
 RowBox@Map[ma,{al}], TBox@@{a} ];

   MakeBoxes[HoldForm[TLI][{a__}, {al__}], TraditionalForm] :=
     SubsuperscriptBox[
 StyleBox["T", FontFamily -> "Times",
               FontWeight -> "Bold"
         ],
 RowBox@Map[ma,{al}], TBox@@{a} ];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TLI | \n "]];
Null
