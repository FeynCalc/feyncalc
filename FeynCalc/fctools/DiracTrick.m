(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracTrick                                                       *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Contraction and simplification rules for Dirac matrices                                        *)

(* ------------------------------------------------------------------------ *)


BeginPackage["HighEnergyPhysics`fctools`DiracTrick`",
             {"HighEnergyPhysics`FeynCalc`"}];

DiracTrick::"usage" =
"DiracTrick[exp] contracts gamma matrices with each other and
performs several simplifications (no expansion, use DiracSimplify for this).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracGamma = MakeContext["CoreObjects","DiracGamma"];
DiracGammaT = MakeContext["CoreObjects","DiracGammaT"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
SUNT = MakeContext["CoreObjects","SUNT"];
Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];
Expanding := Expanding = MakeContext["CoreOptions","Expanding"];
ExpandScalarProduct := ExpandScalarProduct = MakeContext["ExpandScalarProduct"];
FeynCalcInternal := FeynCalcInternal = MakeContext["FeynCalcInternal"];
MemSet := MemSet = MakeContext["MemSet"];
NonCommFreeQ := NonCommFreeQ = MakeContext["NonCommFreeQ"];
PairContract := PairContract = MakeContext["PairContract"];
Spinor := Spinor = MakeContext["CoreObjects","Spinor"];
ExplicitLorentzIndex := ExplicitLorentzIndex = MakeContext["CoreObjects","ExplicitLorentzIndex"];
MakeContext[ FreeQ2, ChargeConjugationMatrix ];

Options[DiracTrick] = {Expanding -> False};

scev[a_, b_] :=
    MemSet[scev[a, b],ExpandScalarProduct[Pair[a,b]]];
coneins[x_] :=
	MemSet[coneins[x],x /. Pair -> PairContract /. PairContract -> Pair];

(*By definition:*)
DiracTrick[] = 1;

(* for time-saving reasons: here NO fci *)
(* RM20120113: added FreeQ, and changed y___ to y__ ..., this fixed
http://www.feyncalc.org/forum/0677.html
*)
DiracTrick[y__ /; FreeQ[{y}, Rule, 1],z_/;Head[z]=!=Rule] :=
    drS[y, z]/.drS -> ds //.dr->drCOs /.
                      drCO -> ds/.  dr->ds/.dr->DOT(*]*);

(*
  Main algorithm:
  1) Simplify expressions involving projectors and slashes (DOT ->  drS)
  2) Check the scheme and then simplify the expressions involving g^5 (drS /. drS ->  ds)
  3) Simplify expressions involving contractions of gamma matrices with momenta or other gammas (ds //. dr -> drCOs)
  4) Again check the sceme and simplify the expressions involving g^5 (twice) (drCO -> ds /.  dr -> ds /.  dr -> DOT)
*)

DiracTrick[x_,r___?OptionQ] :=
    (
    If[ (Expanding /. {r} /. Options[DiracTrick]) === True,
        Expand[ FeynCalcInternal[x] (*/. Dot -> DOT*) /. (*Pair -> PairContract /.*)
                     DOT -> drS /.drS -> ds //. dr -> drCOs/.
                     drCO -> ds /.  dr -> ds /.  dr -> DOT
              ],
        FeynCalcInternal[x] (*/. Dot -> DOT*) /. (*Pair -> PairContract /.*)
             DOT -> drS /.drS -> ds //. dr -> drCOs/.
             drCO -> ds /.  dr -> ds /. dr -> DOT
    (*/.
                  PairContract -> Pair*)
    ]
    );

(*
  RM20120113: commented out, not clear why this should be needed
SetAttributes[DiracTrick, Flat];
*)

ds[x___] :=
    If[ $BreitMaison === True,
        dsBM[x],
        dsNV[x]
    ];

(*If we are not using the BM scheme, we keep the projectors as they are*)
dsNV[x___] :=
    MemSet[dsNV[x], dr[x]]/;  $BreitMaison === False;

(*If we are using the BM scheme, all g^5 in the projectors should be spelled out!*)
dsBM[x___] :=
    MemSet[dsBM[x], dr[x]/.{DiracGamma[6]->(1/2 + DiracGamma[5]/2), DiracGamma[7]->(1/2 - DiracGamma[5]/2) }]/;
        Head[DiracGamma[6]]===DiracGamma && $BreitMaison === True;

(* drdef *)
ds[] = dr[] = 1;
dr[a___,y_SUNT w_,b___] :=
    dr[a, y, w, b](* /; Head[y] === SUNT*);
dr[a___,y_ w_,b___] :=
    coneins[y ds[a,w,b]]/;(NonCommFreeQ[y]&&FreeQ[y,dr]);
dr[a___,y_ ,b___] :=
    coneins[y ds[a,b] ] /;(NonCommFreeQ[y]&&FreeQ[y,dr]);

dr[a_Spinor, b___, c_Spinor, d_Spinor, e___, f_Spinor, g___] :=
    dr[a, b, c] dr[d, e, f, g];

(*Causes infinite recursion!! See above. 19/1-2003 F.Orellana*)
(*dr[a__]:=( ds[a]/.DiracGamma[6]->(1/2 + DiracGamma[5]/2)/.
                  DiracGamma[7]->(1/2 - DiracGamma[5]/2)
         )/;(!FreeQ2[{a}, {DiracGamma[6], DiracGamma[7]}]) &&
            (Head[DiracGamma[6]]===DiracGamma) && $BreitMaison === True;*)


(*These relations between g^5 and the projectors hold in all dimensions and all schemes*)
dr[b___,DiracGamma[5],DiracGamma[5],c___] :=
    ds[ b,c ];
dr[b___,DiracGamma[5],DiracGamma[6],c___] :=
    ds[b,DiracGamma[6],c];
dr[b___,DiracGamma[5],DiracGamma[7],c___] :=
    -ds[b,DiracGamma[7],c];
dr[b___,DiracGamma[6], DiracGamma[5], c___] :=
    ds[b,DiracGamma[6],c];
dr[b___,DiracGamma[7],DiracGamma[5],c___] :=
    -ds[b, DiracGamma[7], c];

(*These are the usual projector properties. They also hold in all schemes*)
dr[___,DiracGamma[6], DiracGamma[7], ___] :=
    0;
dr[___,DiracGamma[7], DiracGamma[6], ___] :=
    0;
dr[b___,DiracGamma[6],DiracGamma[6],c___] :=
    ds[b, DiracGamma[6], c];
dr[b___,DiracGamma[7],DiracGamma[7],c___] :=
    ds[b, DiracGamma[7], c];

(*If we have a projector behind a gamma matrix, we should anticommute them.
  This holds only in four dimensions or in a naive gamma5 scheme *)
dr[b___,DiracGamma[6],DiracGamma[x_[c__],dim_ : 4],d___ ] :=
    ds[b, DiracGamma[x[c], dim], DiracGamma[7],d ]/; ($BreitMaison=!=True || dim === 4);

dr[b___,DiracGamma[7],DiracGamma[x_[c__],dim_ : 4],d___ ] :=
    ds[b, DiracGamma[x[c], dim], DiracGamma[6],d ]/; ($BreitMaison=!=True || dim === 4);


(*In 4 dimensions g^5 always anticommutes with all the other gamma matrices*)
dr[b___,DiracGamma[5],c:DiracGamma[_[_]].. ,d___] :=
    (-1)^Length[{c}] ds[ b,c,DiracGamma[5],d];

(*In D dimensions it depends on the scheme we are using!
  Careful: The following three definitions are scheme dependent!!! *)

(*In the native scheme, g^5 anticommutes with all the other gamma matrices in all dimensions*)
dr[b___,DiracGamma[5],c:DiracGamma[_[__],_].. ,d___] :=
    ( (-1)^Length[{c}] ds[ b,c,DiracGamma[5],d ] ) /;
       ($BreitMaison =!= True && $Larin =!= True);

(*In the BM scheme, the anticommutator is not zero*)
dr[b___,DiracGamma[5],DiracGamma[x_[y__],d_Symbol -4] ,f___] :=
    (ds[ b,DiracGamma[x[y],d-4],DiracGamma[5],f ] ) /;
       ($BreitMaison === True && $Larin =!= True);

dr[b___,DiracGamma[5],DiracGamma[x_[y__],d_Symbol] ,f___] :=
    ( 2 ds[b,DiracGamma[x[y],d-4],DiracGamma[5],f] -
      ds[b,DiracGamma[x[y],d],DiracGamma[5],f] ) /;
       ($BreitMaison === True && $Larin =!= True);


(* o.k., some 4 years after the proposal of M.B., here it is: *)

(* The following relations are true only in the naive g^5 schemes or
if all the involved gamma matrices are four dimensional. They are not
valid for the non-naive schemes. Actually, they are even not applicable
there, since in non-naive schemes all the projects must be written out
explicitly in terms of g^5.
 *)

drS[b___,DiracGamma[7],DiracGamma[_[__], dim1_ : 4] + (n_. mass_),
    xy:DiracGamma[_[__], dim2_ : 4].. , DiracGamma[6], c___] :=
    (n mass drS[b, xy, DiracGamma[6], c])/; NumberQ[n] &&
       OddQ[Length[{xy}]] && NonCommFreeQ[mass] && ($BreitMaison=!=True || (dim1===4 && dim2===4));

drS[b___,DiracGamma[6],DiracGamma[_[__], dim1_ : 4] + (n_. mass_ ),
   xy:DiracGamma[_[__], dim2_ : 4].. , DiracGamma[7], c___] :=
    (n mass drS[b, xy, DiracGamma[7], c]) /; NumberQ[n] &&
      OddQ[Length[{xy}]] && NonCommFreeQ[mass] && ($BreitMaison=!=True || (dim1===4 && dim2===4));

drS[b___,DiracGamma[6],DiracGamma[_[__], dim1_ : 4] + (n_. mass_ ),
   xy:DiracGamma[_[__], dim2_ : 4].. , DiracGamma[6], c___] :=
    (n mass drS[b, xy, DiracGamma[6], c]) /; NumberQ[n] &&
      EvenQ[Length[{xy}]] && NonCommFreeQ[mass] && ($BreitMaison=!=True || (dim1===4 && dim2===4));

drS[b___,DiracGamma[7],DiracGamma[_[__], dim1_ : 4] + (n_. mass_ ),
   xy:DiracGamma[_[__], dim2_ : 4].. , DiracGamma[7], c___] :=
    (n mass drS[b, xy, DiracGamma[7], c]) /; NumberQ[n] &&
      EvenQ[Length[{xy}]] && NonCommFreeQ[mass] && ($BreitMaison=!=True || (dim1===4 && dim2===4));

drS[b___,DiracGamma[6],DiracGamma[_[__], dim_ : 4] + (n_. mass_ ),
    DiracGamma[6], c___] :=
    (n mass drS[b, DiracGamma[6], c] )/; NumberQ[n] && NonCommFreeQ[mass] && ($BreitMaison=!=True || dim===4);

drS[b___,DiracGamma[7],DiracGamma[_[__], dim_ : 4] + (n_. mass_ ),
    DiracGamma[7], c___] :=
    (n mass drS[b, DiracGamma[7], c] )/; NumberQ[n] && NonCommFreeQ[mass] && ($BreitMaison=!=True || dim===4);

drS[b___,DiracGamma[6],DiracGamma[v_[w__], dim_ : 4] + (n_. mass_ ),
    DiracGamma[7], c___] :=
    drS[b, DiracGamma[v[w], dim], DiracGamma[7], c] /; NumberQ[n] &&
      NonCommFreeQ[mass] && ($BreitMaison=!=True || dim===4);

drS[b___,DiracGamma[7],DiracGamma[v_[w__], dim_ : 4] + (n_. mass_),
    DiracGamma[6], c___] :=
    drS[b, DiracGamma[v[w], dim], DiracGamma[6], c] /; NumberQ[n] &&
      NonCommFreeQ[mass] && ($BreitMaison=!=True || dim===4);

drS[b___,DiracGamma[6],DiracGamma[v_[w__], dim1_ : 4] + (n_. mass_ ),
    xy:DiracGamma[_[__], dim2_ : 4].. , DiracGamma[7], c___] :=
    drS[b, DiracGamma[v[w], dim1], xy, DiracGamma[7], c] /; NumberQ[n] &&
           EvenQ[Length[{xy}]] && NonCommFreeQ[mass] && ($BreitMaison=!=True || (dim1===4 && dim2===4));

drS[b___,DiracGamma[7],DiracGamma[v_[w__], dim1_ : 4] + (n_. mass_ ),
    xy:DiracGamma[_[__], dim2_ : 4].. , DiracGamma[6], c___] :=
    drS[b, DiracGamma[v[w], dim1], xy, DiracGamma[6], c] /; NumberQ[n] &&
           EvenQ[Length[{xy}]] && NonCommFreeQ[mass] && ($BreitMaison=!=True || (dim1===4 && dim2===4));

drS[b___,DiracGamma[6],DiracGamma[v_[w__], dim1_ : 4] + (n_. mass_ ),
    xy:DiracGamma[_[__], dim2_ : 4].. , DiracGamma[6], c___] :=
    drS[b, DiracGamma[v[w], dim1], xy, DiracGamma[6], c] /; NumberQ[n] &&
           OddQ[Length[{xy}]] && NonCommFreeQ[mass]  && ($BreitMaison=!=True || (dim1===4 && dim2===4));

drS[b___,DiracGamma[7],DiracGamma[v_[w__], dim1_ : 4] + (n_. mass_),
    xy:DiracGamma[_[__], dim2_ : 4].. , DiracGamma[7], c___] :=
    drS[b, DiracGamma[v[w], dim1], xy, DiracGamma[7], c] /; NumberQ[n] &&
           OddQ[Length[{xy}]] && NonCommFreeQ[mass] && ($BreitMaison=!=True || (dim1===4 && dim2===4));

(* Contractions of neighbouring Dirac matrices in D, 4 and D-4 dimensions     *)
(* ------------------------------------------------------------------------ *)

dr[b___,DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_:  4],
        DiracGamma[LorentzIndex[c_, dim2_ : 4], dim2_: 4], d___] :=
    (PairContract[LorentzIndex[c, dim1],LorentzIndex[c, dim2]]/. PairContract->Pair) ds[ b,d ];

(* Simplification for g^mu ... g_mu where the first and the last
   matrix are in different dimensions                                         *)
(* ------------------------------------------------------------------------ *)

(* D and 4  -> 4 *)
dr[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], d:DiracGamma[__].. ,
          DiracGamma[LorentzIndex[c_]], f___ ] :=
    ds[b, DiracGamma[LorentzIndex[c]], d, DiracGamma[LorentzIndex[c]], f];

(* 4 and D -> 4 *)
dr[ b___, DiracGamma[LorentzIndex[c_]], d:DiracGamma[__].. ,
          DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], f___ ] :=
    ds[b, DiracGamma[LorentzIndex[c]], d, DiracGamma[LorentzIndex[c]], f];

(* D and D-4 -> D-4 *)
dr[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], d:DiracGamma[__].. ,
          DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], f___ ] :=
    ds[b,DiracGamma[LorentzIndex[c, dimD-4], dimD-4],
    d,DiracGamma[LorentzIndex[c, dimD-4], dimD-4], f];

(* D-4 and D -> D-4 *)
dr[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], d:DiracGamma[__].. ,
          DiracGamma[LorentzIndex[c_, dimD_Symbol],dimD_Symbol],f___ ] :=
    ds[b,DiracGamma[LorentzIndex[c, dimD-4], dimD-4],
    d, DiracGamma[LorentzIndex[c, dimD-4], dimD-4],f];

(* 4 and D-4 -> 0 *)
dr[ ___, DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], DiracGamma[__].. ,
          DiracGamma[LorentzIndex[c_]], ___ ] :=
    0;

(* D-4 and 4 -> 0 *)
dr[ ___, DiracGamma[LorentzIndex[c_]], DiracGamma[__].. ,
          DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], ___ ] :=
    0;


(* Simplifications for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in 4 and
     g^nu_i are in D-4 dimensions or vice versa.                                *)
(* ------------------------------------------------------------------------ *)

(* 4, (... D-4 ... ), 4 *)
dr[b___ , DiracGamma[LorentzIndex[c_]],
          ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol-4] , dim_Symbol-4]..,
          DiracGamma[LorentzIndex[c_]], d___] :=
    4 (-1)^Length[{ch}] ds[b,ch, d];

(* D-4, (... 4 ... ), D-4 *)
dr[b___ , DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4],
          ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
          DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
    (dim - 4) (-1)^Length[{ch}] ds[b,ch, d];

(* Simplification for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in D and
    g^nu_i are in 4 dimensions. Applies for n>4, since for n<=4 we have
    explicit expressions in the code                                        *)
(* ------------------------------------------------------------------------ *)

dr[b___ , DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol],
          ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
          DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol], d___] :=
    ds[b,DiracGamma[LorentzIndex[c]], ch, DiracGamma[LorentzIndex[c]], d] +
    (dim - 4) (-1)^Length[{ch}] ds[b,ch, d]/; Length[{ch}]>4;

(* Simplification for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in D and
    g^nu_i are in D-4 dimensions. Applies for n>4, since for n<=4 we have
    explicit expressions in the code                                        *)
(* ------------------------------------------------------------------------ *)

dr[b___ , DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol],
          ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,
            dim_Symbol-4],dim_Symbol-4]..,
          DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol], d___] :=
    ds[b,DiracGamma[LorentzIndex[c,dim-4],dim-4], ch, DiracGamma[LorentzIndex[c,dim-4],dim-4], d] +
    4 (-1)^Length[{ch}] ds[b,ch, d]/; Length[{ch}]>4;


(* Evaluation of g^mu g^nu g_mu for Dirac matrices in different
   dimensions                                                                *)
(* ------------------------------------------------------------------------ *)


(* D, D, D or 4, 4, 4 or D-4, D-4, D-4
   or D, D-4, D or D, 4, D *)
dr[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
          DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim2_ : 4] ,dim2_ : 4],
          DiracGamma[LorentzIndex[c_, dim1_], dim1_ : 4], d___] :=
    (2 - dim1) ds[b,DiracGamma[x[y, dim2], dim2], d]/;
                    (dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
                    MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
                    MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D-4 or 4, D, 4 *)
dr[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
          DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim2_Symbol], dim2_Symbol],
          DiracGamma[LorentzIndex[c_, dim1_ : 4],  dim1_ : 4], d___] :=
    - dim1 ds[b,DiracGamma[x[y,dim2],dim2],d] +
    2 ds[b,DiracGamma[x[y,dim1],dim1],d]/; (dim1 === dim2-4 || dim1 ===4);

(* Evaluation of g^mu g^nu g^rho g_mu for Dirac matrices in different
   dimensions                                                                *)
(* ------------------------------------------------------------------------ *)


(* D, D, D, D or 4, 4, 4, 4 or D-4, D-4, D-4, D-4
    or D, D-4, D-4, D or D, 4, 4, D *)
dr[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
          DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_ : 4] ,dim2_ : 4],
          DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_ : 4] ,dim2_ : 4],
          DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
    (dim1 - 4) ds[b,DiracGamma[x1[y1, dim2], dim2],
                    DiracGamma[x2[y2, dim2], dim2], d] +
                    4 ds[b,Pair[x1[y1,dim2],x2[y2,dim2]], d]/;
                    (dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
                    MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
                    MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D, D-4 or 4, D, D, 4 *)
dr[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
          DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_Symbol], dim2_Symbol],
          DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_Symbol], dim2_Symbol],
          DiracGamma[LorentzIndex[c_, dim1_: 4], dim1_ : 4], d___] :=
    dim1 ds[b,DiracGamma[x1[y1, dim2], dim2],DiracGamma[x2[y2, dim2], dim2], d] -
    2 ds[b,DiracGamma[x1[y1, dim1], dim1],DiracGamma[x2[y2, dim2], dim2], d] +
    2 ds[b,DiracGamma[x2[y2, dim1], dim1],DiracGamma[x1[y1, dim2], dim2], d]/;
    (dim1 === dim2-4 || dim1 ===4);


(* Evaluation of g^mu g^nu g^rho g^sigma g_mu for Dirac matrices in different
   dimensions                                                                *)
(* ------------------------------------------------------------------------ *)

(* D, D, D, D, D or 4, 4, 4, 4, 4 or D-4, D-4, D-4, D-4, D-4
    or D, D-4, D-4, D-4, D or D, 4, 4, 4, D *)
dr[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
          DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_ : 4] , dim2_ : 4],
          DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_ : 4] , dim2_ : 4],
          DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_ : 4] , dim2_ : 4],
          DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
    -(dim1 - 4) ds[b, DiracGamma[x1[y1, dim2], dim2],
                      DiracGamma[x2[y2, dim2], dim2],
                      DiracGamma[x3[y3, dim2], dim2], d] -
                    2 ds[b, DiracGamma[x3[y3, dim2], dim2],
                            DiracGamma[x2[y2, dim2], dim2],
                            DiracGamma[x1[y1, dim2], dim2], d]/;
                            (dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
                            MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
                            MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D, D, D-4 or 4, D, D, D, 4 *)
dr[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
          DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_Symbol], dim2_Symbol],
          DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_Symbol], dim2_Symbol],
          DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_Symbol], dim2_Symbol],
          DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
    -dim1 ds[b, DiracGamma[x1[y1, dim2], dim2],
              DiracGamma[x2[y2, dim2], dim2],
              DiracGamma[x3[y3, dim2], dim2], d] +
    2 ds[b, DiracGamma[x1[y1, dim1], dim1],
          DiracGamma[x2[y2, dim2], dim2],
          DiracGamma[x3[y3, dim2], dim2], d] -
    2 ds[b, DiracGamma[x2[y2, dim1], dim1],
          DiracGamma[x1[y1, dim2], dim2],
          DiracGamma[x3[y3, dim2], dim2], d] +
    2 ds[b, DiracGamma[x3[y3, dim1], dim1],
          DiracGamma[x1[y1, dim2], dim2],
          DiracGamma[x2[y2, dim2], dim2], d]/;
            (dim1 === dim2-4 || dim1 ===4);

(* Evaluation of g^mu g^nu g^rho g^sigma g^tau g_mu for Dirac matrices
   in different dimensions                                                    *)
(* ------------------------------------------------------------------------ *)

(* D, D, D, D, D, D or 4, 4, 4, 4, 4, 4 or D-4, D-4, D-4, D-4, D-4, D-4
    or D, D-4, D-4, D-4, D-4, D or D, 4, 4, 4, 4, D *)
dr[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
          DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_ : 4] , dim2_ : 4],
          DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_ : 4] , dim2_ : 4],
          DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_ : 4] , dim2_ : 4],
          DiracGamma[(x4: LorentzIndex | ExplicitLorentzIndex | Momentum)[y4_, dim2_ : 4] , dim2_ : 4],
          DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
    (dim1 - 4) ds[b, DiracGamma[x1[y1, dim2], dim2],
                     DiracGamma[x2[y2, dim2], dim2],
                     DiracGamma[x3[y3, dim2], dim2],
                     DiracGamma[x4[y4, dim2], dim2], d] +
                   2 ds[b, DiracGamma[x3[y3, dim2], dim2],
                           DiracGamma[x2[y2, dim2], dim2],
                           DiracGamma[x1[y1, dim2], dim2],
                           DiracGamma[x4[y4, dim2], dim2], d] +
                 2 ds[b, DiracGamma[x4[y4, dim2], dim2],
                           DiracGamma[x1[y1, dim2], dim2],
                           DiracGamma[x2[y2, dim2], dim2],
                           DiracGamma[x3[y3, dim2], dim2], d]/;
                           (dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
                           MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
                           MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D, D, D, D-4 or 4, D, D, D, D, 4 *)
dr[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
          DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_Symbol], dim2_Symbol],
          DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_Symbol], dim2_Symbol],
          DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_Symbol], dim2_Symbol],
          DiracGamma[(x4: LorentzIndex | ExplicitLorentzIndex | Momentum)[y4_, dim2_Symbol], dim2_Symbol],
          DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
    dim1  ds[b, DiracGamma[x1[y1, dim2], dim2],
              DiracGamma[x2[y2, dim2], dim2],
              DiracGamma[x3[y3, dim2], dim2],
              DiracGamma[x4[y4, dim2], dim2], d] -
    2 ds[b, DiracGamma[x1[y1, dim1], dim1],
          DiracGamma[x2[y2, dim2], dim2],
          DiracGamma[x3[y3, dim2], dim2],
          DiracGamma[x4[y4, dim2], dim2], d] +
    2 ds[b, DiracGamma[x2[y2, dim1], dim1],
          DiracGamma[x1[y1, dim2], dim2],
          DiracGamma[x3[y3, dim2], dim2],
          DiracGamma[x4[y4, dim2], dim2], d] -
    2 ds[b, DiracGamma[x3[y3, dim1], dim1],
          DiracGamma[x1[y1, dim2], dim2],
          DiracGamma[x2[y2, dim2], dim2],
          DiracGamma[x4[y4, dim2], dim2], d] +
    2 ds[b, DiracGamma[x4[y4, dim1], dim1],
          DiracGamma[x1[y1, dim2], dim2],
          DiracGamma[x2[y2, dim2], dim2],
          DiracGamma[x3[y3, dim2], dim2], d]/;
            (dim1 === dim2-4 || dim1 ===4);

(* Evaluation of a string of 4 dimensional Dirac matrices
   g^mu g^nu_1 ... g^nu_i g_mu  -> -2 g^nu_i ... g^nu_1,
   where i is odd *)
dr[ b___,  DiracGamma[LorentzIndex[c_]],
        ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
        DiracGamma[LorentzIndex[c_]], f___ ] :=
    -2 ds @@ Join[ {b},Reverse[{ch}],{f} ] /; OddQ[Length[{ch}]];


(* Evaluation of a string of 4 dimensional Dirac matrices
   g^mu g^nu_1 ... g^nu_i g_mu  ->
   2 g^nu_i-1 ... g^nu_1 g^nu_i + 2 g^nu_i g^nu_1 ... g^nu_i-1,
   where i is even *)
dr[ b___,  DiracGamma[LorentzIndex[c_]],
        ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
        end : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]],
        DiracGamma[LorentzIndex[c_]], f___ ] :=
    ( 2 ds @@ Join[ {b},Reverse[{ch}],{end} ] +
     2 ds[ b,end,ch,f ]
    )/; OddQ[Length[{ch}]];

(* Slash(p).Slash(p), where both objects have the same dimension *)
dr[b___,DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],
        DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],d___ ] :=
    scev[Momentum[c,dim],Momentum[c,dim]] ds[b,d];

(* Simplifications for Slash(p) g^nu_1 ... g^nu_n Slash(p) where the slashes
   are in 4 and g^nu_i are in D-4 dimensions or vice versa.                 *)
(* ------------------------------------------------------------------------ *)

(* 4, (... D-4 ... ), 4 *)
dr[b___ , DiracGamma[Momentum[c_]],
          ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol-4] , dim_Symbol-4]..,
          DiracGamma[Momentum[c_]], d___] :=
    (-1)^Length[{ch}] scev[Momentum[c],Momentum[c]] ds[b,ch, d];

(* D-4, (... 4 ... ), D-4 *)
dr[b___ , DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4],
          ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
          DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
    (-1)^Length[{ch}] scev[Momentum[c, dim-4],Momentum[c, dim-4]] ds[b,ch, d];

(* Simplification for Slash(p) g^nu_1 ... g^nu_n Slash(p) where the slashes
   are in D and g^nu_i are in 4 dimensions. This applies for n>1.           *)
(* ------------------------------------------------------------------------ *)

dr[b___ , DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol],
          ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
          DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol], d___] :=
    ds[b,DiracGamma[Momentum[c]], ch, DiracGamma[Momentum[c]], d] +
    (-1)^Length[{ch}] scev[Momentum[c, dim-4],Momentum[c, dim-4]] ds[b,ch, d]/; Length[{ch}]>1;

(* Simplification for Slash(p) g^nu_1 ... g^nu_n Slash(p) where the slashes are
   in D and g^nu_i are in D-4 dimensions. This applies for n>1.             *)
(* ------------------------------------------------------------------------ *)

dr[b___ , DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol],
          ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,
            dim_Symbol-4],dim_Symbol-4]..,
          DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol], d___] :=
    ds[b,DiracGamma[Momentum[c,dim-4],dim-4], ch, DiracGamma[Momentum[c,dim-4],dim-4], d] +
    (-1)^Length[{ch}] scev[Momentum[c],Momentum[c]]  ds[b,ch, d]/; Length[{ch}]>1;


(* Evaluation of Slash(p) g^nu Slash(p) for Dirac matrices in different
   dimensions                                                               *)
(* ------------------------------------------------------------------------ *)

dr[b___ , DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4],
          DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim2_ : 4] ,dim2_ : 4],
          DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4], d___] :=
    - scev[Momentum[c,dim1],Momentum[c,dim1]] ds[b,DiracGamma[x[y, dim2], dim2], d] +
    2 ds[b, Pair[Momentum[c,dim1],x[y,dim2]], DiracGamma[Momentum[c, dim1], dim1], d]/;
                    (dim1===dim2 || dim2 === dim1-4 || dim2 ===4 || dim1 === dim2-4 || dim1 ===4) &&
                    MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
                    MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* #################################################################### *)
(*                             Main33                                 *)
(* #################################################################### *)

(* If we have a mixed expression with gamma and SU(N) matrices, factor the SU(N) matrices out *)
   dr[ a___,b_,c:SUNT[_].. ,d___] :=
       dr[ a, c, b, d ] /; FreeQ2[b, {SUNT}];

   HoldPattern[dr[ a___,b_ dr[c:(SUNT[_])..], d___]] :=
       ( dr[c] dr[a, b, d] )/;FreeQ[{a, b, d}, SUNT];
   dr[ SUNT[i_], b___ ] :=
       (SUNT[i] ds[b]) /; FreeQ[{b}, SUNT];
   dr[ b__, SUNT[i_] ] :=
       (SUNT[i] ds[b]) /; FreeQ[{b}, SUNT];
   dr[ a__, b:SUNT[_].. ] :=
       (ds[b] ds[a])/; FreeQ[{a}, SUNT];
   dr[ b:SUNT[_].., a__ ] :=
       (ds[b] ds[a])/; FreeQ[{a}, SUNT];



(* #################################################################### *)
(*                             Main33a                                 *)
(* #################################################################### *)

(*Properties of the charge conjugation matrix for spinors. *)

   dr[ a___, ChargeConjugationMatrix, ChargeConjugationMatrix, b___ ] :=
       -dr[a, b];
   dr[ a___, ChargeConjugationMatrix, DiracGamma[5], b___ ] :=
       dr[a, DiracGammaT[5], ChargeConjugationMatrix, b];
   dr[ a___, ChargeConjugationMatrix, DiracGamma[6], b___ ] :=
       dr[a, DiracGammaT[6], ChargeConjugationMatrix, b];
   dr[ a___, ChargeConjugationMatrix, DiracGamma[7], b___ ] :=
       dr[a, DiracGammaT[7], ChargeConjugationMatrix, b];
   dr[ a___, ChargeConjugationMatrix, DiracGamma[x_], b___ ] :=
       -dr[a, DiracGammaT[x], ChargeConjugationMatrix, b] /; !NumberQ[x];
   dr[ a___, ChargeConjugationMatrix, DiracGammaT[x_], b___ ] :=
       -dr[a, DiracGamma[x], ChargeConjugationMatrix, b] /; !NumberQ[x];

(* #################################################################### *)
(*                             Main34                                 *)
(* #################################################################### *)

drCOs[x___] :=
    If[ $BreitMaison === True,
        drCOBM[x],
        drCONV[x]
    ];

drCONV[x___] :=
    MemSet[drCONV[x], drCO[x]]/;  $BreitMaison === False;
drCOBM[x___] :=
    MemSet[drCOBM[x], drCO[x]]/; $BreitMaison === True;

(* Dirac contraction rules *) (*drCOdef*)

(* g^mu g^nu_1 ... g^nu_n g_mu, where g^mu is in 4 or D-4
   and g^nu_i are in D dimensions is simplfied by repeatedly
   applying anticommuation relations. Applies for n>5,
   since for n<=5 we have explicit expressions in the code *)

   drCO[b___, DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4],
            ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
            ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
            DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
       (-drCO[b, DiracGamma[LorentzIndex[c, dim-4],dim-4], ch1, DiracGamma[LorentzIndex[c, dim-4], dim-4], ch2, d]
       + 2 drCO[b, DiracGamma[h[x,dim-4],dim-4], ch1, d] )/.drCO->ds;

   drCO[b___, DiracGamma[LorentzIndex[c_]],
            ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
            ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
            DiracGamma[LorentzIndex[c_]], d___] :=
       (-drCO[b, DiracGamma[LorentzIndex[c]], ch1, DiracGamma[LorentzIndex[c]], ch2, d]
       + 2 drCO[b, DiracGamma[h[x]], ch1, d] )/.drCO->ds;

(* Slash(p) g^nu_1 ... g^nu_n Slash(p), where the slashes
   are in 4 or D-4 and g^nu_i are in D dimensions is simplfied
   by repeatedly applying anticommuation relations.             *)

   drCO[b___, DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4],
            ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
            ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
            DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
       (-drCO[b, DiracGamma[Momentum[c, dim-4],dim-4], ch1, DiracGamma[Momentum[c, dim-4], dim-4], ch2, d]
       + 2 drCO[b,Pair[Momentum[c,dim-4],h[x,dim-4]], DiracGamma[Momentum[c, dim-4],dim-4], ch1, d] )/.drCO->ds;

   drCO[b___, DiracGamma[Momentum[c_]],
            ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
            ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
            DiracGamma[Momentum[c_]], d___] :=
       (-drCO[b, DiracGamma[Momentum[c]], ch1, DiracGamma[Momentum[c]], ch2, d]
       + 2 drCO[b,Pair[Momentum[c],h[x]], DiracGamma[Momentum[c]], ch1, d] )/.drCO->ds;

(* g^mu ... g^nu g^rho g_mu, where g^mu is in D, 4, or D-4
   and g^nu and g^rho are in different dimensions is simplfied
   by repeatedly applying anticommuation relations.            *)

    drCO[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
          w___,
          DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim3_ : 4] ,dim3_ : 4],
          DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
        ((
          -ds[b,  DiracGamma[LorentzIndex[c, dim1], dim1], w,
                    DiracGamma[LorentzIndex[c, dim1], dim1],
                    DiracGamma[x2[y2,dim3],dim3], d]
                + 2 ds[b, DiracGamma[LorentzIndex[c, dim1], dim1], w,
                            Pair [LorentzIndex[c, dim1], x2[y2,dim3]],d]))/;
                        (dim1===dim3 || dim3 === dim1-4 || dim1 === dim3-4 || dim3 === 4 || dim1 === 4) &&
                        MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
                        MatchQ[dim3, _Symbol | _Symbol-4 | 4 ] &&
                        !MatchQ[{w, DiracGamma[x2[y2,dim3],dim3]},{DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim3] , dim3]..} |
                            {___, DiracGamma[a__], ___, DiracGamma[a__], ___}];

(* Slash(p) ... g^nu g^rho Slash(p), where the slashes are in D, 4, or D-4
   and g^nu and g^rho are in different dimensions is simplfied
   by repeatedly applying anticommuation relations.            *)

    drCO[b___ , DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4],
          w___,
          DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim3_ : 4] ,dim3_ : 4],
          DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4], d___] :=
        ((
          -ds[b,  DiracGamma[Momentum[c, dim1], dim1], w,
                    DiracGamma[Momentum[c, dim1], dim1],
                    DiracGamma[x2[y2,dim3],dim3], d]
                + 2 ds[b, DiracGamma[Momentum[c, dim1], dim1], w,
                            Pair [Momentum[c, dim1], x2[y2,dim3]], d]))/;
                        (dim1===dim3 || dim3 === dim1-4 || dim1 === dim3-4 || dim3 === 4 || dim1 === 4) &&
                        MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
                        MatchQ[dim3, _Symbol | _Symbol-4 | 4 ] &&
                        !MatchQ[{w, DiracGamma[x2[y2,dim3],dim3]},{DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim3] , dim3]..} |
                            {___, DiracGamma[a__], ___, DiracGamma[a__], ___}];

(* Simplification for Slash(p) g^nu_1 ... g^nu_n Slash(p) where all
   matrices are in D, 4 or D-4 dimensions. The
   formula is given in Eq 2.10 of R. Mertig, M. Boehm,
   A. Denner. Comp. Phys. Commun., 64 (1991)                *)
   drCO[b___, DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],
              ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_ : 4], dim_ : 4]..,
              DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],f___
       ] :=
       Block[ {iVar, len = Length[{ch}]},
           (-1)^len scev[Momentum[c, dim],Momentum[c, dim]] ds[b,ch,f]
           + 2 Sum[(-1)^(iVar+1) coneins[ Pair[Momentum[c, dim],{ch}[[iVar,1]] ]
                  * ds@@Join[{b},Drop[{ch},{iVar, iVar}],{DiracGamma[Momentum[c, dim],dim],f}]
                                          ],{iVar, 1,len}]
       ]/;
                   (Length[{ch}]>0) && MatchQ[dim, _Symbol | _Symbol-4 | 4 ];

(* Simplification for g^mu g^nu_1 ... g^nu_n g_mu where all
   matrices are  in D or D-4 dimensions. Applies for n>5, since
   for n<=5 we have explicit expressions in the code. The
   formula is given in Eq 2.9 of R. Mertig, M. Boehm,
   A. Denner. Comp. Phys. Commun., 64 (1991)                *)
   drCO[ b___,DiracGamma[LorentzIndex[c_,dim_],dim_],
        ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,
        dim_],dim_]..,
        DiracGamma[LorentzIndex[c_,dim_],dim_],f___ ] :=
       Block[ {iVar,jVar,len = Length[{ch}],dsTemp},
           (((-1)^len ( dim - 2 len ) dsTemp[b,ch,f] - 4 (-1)^len *
            Sum[ (-1)^(jVar-iVar) *  Pair[{ch}[[iVar,1]],
                {ch}[[jVar,1]] ]*dsTemp@@Join[{b},
                Delete[{ch}, {{iVar},{jVar}}], {f}],{iVar,1,len-1},{jVar,iVar+1,len}
            ])//coneins)/.dsTemp->ds/.Pair->scev
       ] /;(Length[{ch}]>4) && MatchQ[dim, _Symbol | _Symbol-4 ];

(* ************************************************************** *)
 SetAttributes[drS,Flat];
(* ************************************************************** *)
 SetAttributes[dr,Flat];   (* quite important!!! *)
(* ************************************************************** *)

End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0,
    WriteString["stdout", "DiracTrick | \n "]
];
Null
