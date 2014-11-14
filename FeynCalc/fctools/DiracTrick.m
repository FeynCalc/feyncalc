(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracTrick *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: contraction and simplification rules for gamma matrices *)

(* ------------------------------------------------------------------------ *)

(* NonCommQ replaced with NonCommFreeQ everywhere due to change (fix) of
   definitions of these functions. F.Orellana, 13/9-2002 *)

BeginPackage["HighEnergyPhysics`fctools`DiracTrick`",
             {"HighEnergyPhysics`FeynCalc`"}];

DiracTrick::"usage"=
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
FeynCalcInternal  := FeynCalcInternal =  MakeContext["FeynCalcInternal"];
MemSet := MemSet = MakeContext["MemSet"];
NonCommFreeQ := NonCommFreeQ = MakeContext["NonCommFreeQ"];
PairContract := PairContract          = MakeContext["PairContract"];
Spinor := Spinor = MakeContext["CoreObjects","Spinor"];
ExplicitLorentzIndex:= ExplicitLorentzIndex = MakeContext["CoreObjects","ExplicitLorentzIndex"];
MakeContext[ FreeQ2, ChargeConjugationMatrix ];

Options[DiracTrick] = {Expanding -> False};

scev[a_, b_] := MemSet[scev[a, b],ExpandScalarProduct[Pair[a,b]]];
coneins[x_]  := x /. Pair -> PairContract /. PairContract -> Pair;

(*By definition:*)
DiracTrick[]=1;

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

DiracTrick[x_,r___?OptionQ] :=(
  If[(Expanding /. {r} /. Options[DiracTrick]) === True,
     Expand[ FeynCalcInternal[x] (*/. Dot -> DOT*) /. (*Pair -> PairContract /.*)
                  DOT -> drS /.drS -> ds //. dr -> drCOs/.
                  drCO -> ds /.  dr -> ds /.  dr -> DOT
           ],
             FeynCalcInternal[x] (*/. Dot -> DOT*) /. (*Pair -> PairContract /.*)
                  DOT -> drS /.drS -> ds //. dr -> drCOs/.
                  drCO -> ds /.  dr -> ds /. dr -> DOT (*/.
                  PairContract -> Pair*)
    ]
);

(*
  RM20120113: commented out, not clear why this should be needed
SetAttributes[DiracTrick, Flat];
*)



(*If we are not using the BM scheme, we keep the projectors as they are*)
ds[x__] := MemSet[ds[x,$BreitMaison, $Larin], dr[x]] /; ((!FreeQ2[{x}, {DiracGamma[6], DiracGamma[7]}]) &&
            (Head[DiracGamma[6]]===DiracGamma) && $BreitMaison === True) =!= True;
                                (*Condition added 19/1-2003 by F.Orellana to not have
                                  definition below cause infinite recursion.*)

(*If we are using the BM scheme, all g^5 in the projectors should be spelled out!*)
ds[x__] := MemSet[ds[x,$BreitMaison, $Larin], dr[x]/.DiracGamma[6]->(1/2 + DiracGamma[5]/2)/.
                  DiracGamma[7]->(1/2 - DiracGamma[5]/2)] /; ((!FreeQ2[{x}, {DiracGamma[6], DiracGamma[7]}]) &&
            (Head[DiracGamma[6]]===DiracGamma) && $BreitMaison === True) === True;

(* drdef *)

ds[] = dr[]=1;
dr[a___,y_SUNT w_,b___] := dr[a, y, w, b](* /; Head[y] === SUNT*);
dr[a___,y_ w_,b___] := coneins[y ds[a,w,b]]/;(NonCommFreeQ[y]&&FreeQ[y,dr]);
dr[a___,y_ ,b___]   := coneins[y ds[a,b] ] /;(NonCommFreeQ[y]&&FreeQ[y,dr]);

dr[a_Spinor, b___, c_Spinor, d_Spinor, e___, f_Spinor, g___]:=
 dr[a, b, c] dr[d, e, f, g];

(*Causes infinite recursion!! See above. 19/1-2003 F.Orellana*)
(*dr[a__]:=( ds[a]/.DiracGamma[6]->(1/2 + DiracGamma[5]/2)/.
                  DiracGamma[7]->(1/2 - DiracGamma[5]/2)
         )/;(!FreeQ2[{a}, {DiracGamma[6], DiracGamma[7]}]) &&
            (Head[DiracGamma[6]]===DiracGamma) && $BreitMaison === True;*)


(*These relations between g^5 and the projectors hold in all dimensions and all schemes*)
dr[b___,DiracGamma[5],DiracGamma[5],c___]:= ds[ b,c ];
dr[b___,DiracGamma[5],DiracGamma[6],c___]:= ds[b,DiracGamma[6],c];
dr[b___,DiracGamma[5],DiracGamma[7],c___]:=-ds[b,DiracGamma[7],c];
dr[b___,DiracGamma[6], DiracGamma[5], c___]:=ds[b,DiracGamma[6],c];
dr[b___,DiracGamma[7],DiracGamma[5],c___] := -ds[b, DiracGamma[7], c];

(*These are the usual projector properties. They also hold in all schemes*)
dr[___,DiracGamma[6], DiracGamma[7], ___] := 0;
dr[___,DiracGamma[7], DiracGamma[6], ___] := 0;
dr[b___,DiracGamma[6],DiracGamma[6],c___] :=  ds[b, DiracGamma[6], c];
dr[b___,DiracGamma[7],DiracGamma[7],c___] :=  ds[b, DiracGamma[7], c];

(*If we have a projector behind a gamma matrix, we should anticommute them.
  This holds only in four dimensions or in a naive gamma5 scheme *)
dr[b___,DiracGamma[6],DiracGamma[x_[c__],dim_ : 4],d___ ]:=
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

(* Contractions of neighbouring Dirac matrices in D, 4 and D-4 dimensions 	*)
(* ------------------------------------------------------------------------ *)

(* D and D *)
dr[b___,DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol],
        DiracGamma[LorentzIndex[c_, dim_Symbol ], dim_Symbol], d___] :=
    dim ds[ b,d ];

(* 4 and 4 *)
dr[b___,DiracGamma[LorentzIndex[c_]], DiracGamma[LorentzIndex[c_]], d___] :=
    4 ds[ b,d ];

(* D-4 and D-4 *)
dr[b___,DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4],
        DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
    (dim-4) ds[ b,d ];

(* D and 4 *)
dr[b___,DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol],
        DiracGamma[LorentzIndex[c_]], d___] :=
    4 ds[ b,d ];


(* 4 and D *)
dr[b___,DiracGamma[LorentzIndex[c_]],
        DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol], d___] :=
    4 ds[ b,d ];

(* D-4 and D *)
dr[b___,DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4],
        DiracGamma[LorentzIndex[c_, dim_], dim_], d___] :=
	(dim-4) ds[ b,d ];

(* D and D-4 *)
dr[b___,DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol],
        DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
	(dim-4) ds[ b,d ];

(* D-4 and 4 *)
dr[___,DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4],
        DiracGamma[LorentzIndex[c_]], ___] :=
	0;

(* 4 and D-4 *)
dr[___,DiracGamma[LorentzIndex[c_]],
        DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4], ___] :=
	0;

(* ------------------------------------------------------------------------ *)


fdim[]=4;    (* fdimdef *)
fdim[dimi_]:=dimi;

dcheck[dii_, diii__] := MemSet[dcheck[dii,diii], If[Head[dii]===Symbol, True, If[Union[{dii, diii}]==={dii}, True, False]]];

(* Simplification for g^mu ... g_mu where the first and the last
   matrix are in different dimensions 										*)
(* ------------------------------------------------------------------------ *)

(* D and 4  -> 4 *)
dr[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], d:DiracGamma[__].. ,
		  DiracGamma[LorentzIndex[c_]], f___ ]:=
	ds[b, DiracGamma[LorentzIndex[c]], d, DiracGamma[LorentzIndex[c]], f];

(* 4 and D -> 4 *)
dr[ b___, DiracGamma[LorentzIndex[c_]], d:DiracGamma[__].. ,
		  DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], f___ ]:=
	ds[b, DiracGamma[LorentzIndex[c]], d, DiracGamma[LorentzIndex[c]], f];

(* D and D-4 -> D-4 *)
dr[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], d:DiracGamma[__].. ,
          DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], f___ ]:=
	ds[b,DiracGamma[LorentzIndex[c, dimD-4], dimD-4],
	d,DiracGamma[LorentzIndex[c, dimD-4], dimD-4], f];

(* D-4 and D -> D-4 *)
dr[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], d:DiracGamma[__].. ,
    	  DiracGamma[LorentzIndex[c_, dimD_Symbol],dimD_Symbol],f___ ]:=
	ds[b,DiracGamma[LorentzIndex[c, dimD-4], dimD-4],
	d, DiracGamma[LorentzIndex[c, dimD-4], dimD-4],f];

(* 4 and D-4 -> 0 *)
dr[ ___, DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], d:DiracGamma[__].. ,
          DiracGamma[LorentzIndex[c_]], ___ ]:=
	0;

(* D-4 and 4 -> 0 *)
dr[ ___, DiracGamma[LorentzIndex[c_]], d:DiracGamma[__].. ,
          DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], ___ ]:=
	0;


(* Simplifications for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in 4 and
 	g^nu_i are in D-4 dimensions or vice versa.								*)
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



(* Evaluation of g^mu g^nu g_mu for Dirac matrices in different
   dimensions																*)
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
   dimensions																*)
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
   dimensions																*)
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
   in different dimensions													*)
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

(*g^mu g^nu g^rho g^sigma g^tau g^kappa g_mu for arbitrary dimensions*)
dr[b___,DiracGamma[LorentzIndex[c_,dI___],dI___],
        DiracGamma[x1_[y1__],d1___], DiracGamma[x2_[y2__],d2___],
        DiracGamma[x3_[y3__],d3___], DiracGamma[x4_[y4__],d4___],
        DiracGamma[x5_[y5__],d5___],
        DiracGamma[LorentzIndex[c_,dI___],dI___],d___
  ] := ( 2 ds[b,DiracGamma[x2[y2],d2], DiracGamma[x3[y3],d3],
                DiracGamma[x4[y4],d4], DiracGamma[x5[y5],d5],
                DiracGamma[x1[y1],d1],
            d] -
         2 ds[b,DiracGamma[x1[y1],d1], DiracGamma[x4[y4],d4],
                DiracGamma[x3[y3],d3], DiracGamma[x2[y2],d2],
                DiracGamma[x5[y5],d5],
            d] -
         2 ds[b,DiracGamma[x1[y1],d1], DiracGamma[x5[y5],d5],
                DiracGamma[x2[y2],d2], DiracGamma[x3[y3],d3],
                DiracGamma[x4[y4],d4],
            d] -
      (fdim[dI]-4) ds[b,DiracGamma[x1[y1],d1], DiracGamma[x2[y2],d2],
                        DiracGamma[x3[y3],d3], DiracGamma[x4[y4],d4],
                        DiracGamma[x5[y5],d5],
                    d] ) /; dcheck[dI, d1,d2,d3,d4,d5];

(* Slash(p)*Slash(p), where p in the first slash is in D1 and in the second one in D2 dimensions.
   The dimensions of the gammas (no g^5 here!) doesn't matter  *)
dr[b___,DiracGamma[Momentum[c_,dim1___],___],
        DiracGamma[Momentum[c_,dim2___],___],d___ ] :=
        scev[Momentum[c,dim1],Momentum[c,dim2]] ds[b,d];

(* Slash(p)*[odd # of gammas (no g^5 here!)]*Slash(p), everything is in 4 dimensions  *)
dr[ b___,DiracGamma[LorentzIndex[c_]],d:DiracGamma[_[_]].. ,
         DiracGamma[LorentzIndex[c_]],f___ ] :=
    -2 ds @@ Join[ {b},Reverse[{d}],{f} ] /; OddQ[Length[{d}]];

(*Slash(p) Slash(k) Slash(p), where gamma (no g^5 here!) in the first slash is in D1 dimensions, in the second one D2 dimensions and in the third one
in D3 dimensions. The momentum vectors are all in 4 dimensions*)
dr[ b___,DiracGamma[Momentum[c__],dim___],
         DiracGamma[Momentum[x__],dii___],
         DiracGamma[Momentum[c__],___],d___ ] := (
2 scev[Momentum[c],Momentum[x]] ds[b,DiracGamma[Momentum[c],dim],d]
- scev[Momentum[c],Momentum[c]] ds[b,DiracGamma[Momentum[x],dii],d]
                                                  );


(* #################################################################### *)
(*                             Main33                                 *)
(* #################################################################### *)

(* If we have a mixed expression with gamma and SU(N) matrices, factor the SU(N) matrices out *)
   dr[ a___,b_,c:SUNT[_].. ,d___] :=
     dr[ a, c, b, d ] /; FreeQ2[b, {SUNT}];

   HoldPattern[dr[ a___,b_ dr[c:(SUNT[_])..], d___]]:=
     ( dr[c] dr[a, b, d] )/;FreeQ[{a, b, d}, SUNT];

   dr[ SUNT[i_], b___ ] := (SUNT[i] ds[b]) /; FreeQ[{b}, SUNT];

   dr[ b__, SUNT[i_] ] := (SUNT[i] ds[b]) /; FreeQ[{b}, SUNT];

   dr[ a__, b:SUNT[_].. ]:=(ds[b] ds[a])/; FreeQ[{a}, SUNT];

   dr[ b:SUNT[_].., a__ ]:=(ds[b] ds[a])/; FreeQ[{a}, SUNT];



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

   drCOs[x___] := MemSet[ drCOs[x,$BreitMaison, $Larin], drCO[x] ];    (*drCOsdef*)
(* Dirac contraction rules *) (*drCOdef*)

(*g^mu g^i1 .... g^in g_mu, where all matrices are in D-4 dimensions
  This is evaluated in the same manner as for D dimensions          *)
   drCO[ b___,DiracGamma[LorentzIndex[c_,di_Symbol-4],di_Symbol-4],
         d:DiracGamma[_[_,di_Symbol-4], di_Symbol-4].. ,
         DiracGamma[LorentzIndex[c_,di_Symbol-4],di_Symbol-4],f___
       ]:= (drCO @@  ( { b, DiracGamma[LorentzIndex[c,di-4], di-4],
                         d, DiracGamma[LorentzIndex[c,di-4], di-4],
                         f } /. di -> (di + 4)
                     )) /. di -> (di-4);

(*g^mu ... g^nu g_mu = -2 g^mu ... g_mu g^nu + 2 g^nu ...
 where on the LHS g^mu and g_mu are in D-4 dimensions and
 g^nu is in D dimensions*)
   drCO[ b___,DiracGamma[lv_[c_,di_Symbol-4],di_Symbol-4], w___,
              DiracGamma[ww_[y__],dim___],
              DiracGamma[lv_[c_,di_Symbol-4],di_Symbol-4], z___] :=
   (Print["rdCOCheck"];
         -drCO[ b, DiracGamma[lv[c,di-4],di-4],w,
             DiracGamma[lv[c,di-4],di-4],
             DiracGamma[ww[y],dim],z
        ] + 2 drCO[b, DiracGamma[ww[y],di-4], w,z] )/.drCO->ds;

(*g^mu g^i1 ... g^in g^nu g_mu = 2 g^in .... g^i1 g^nu + 2 g^nu g^in .... g^i1,
where all the matrices (no g^5 here!) are in 4 dimensions and n is od*)
   drCO[ b___,DiracGamma[LorentzIndex[c_]],d:DiracGamma[_[__]].. ,
         DiracGamma[x_[y__]],DiracGamma[LorentzIndex[c_]],f___ ] :=
       ( 2 ds @@ Join[ {b},Reverse[{d}],{DiracGamma[x[y]],f} ] +
         2 ds[ b,DiracGamma[x[y]],d,f ]
        ) /; OddQ[Length[{d}]];

(*Slash(p) g^i1 ... g^in Slash(p), where the first slash is in D or 4 dimensions,
  while the second slash and g^ii are in D, D-4 or 4 dimensions.
  The formula is given in Eq 2.10 of R.
  Mertig, M. Boehm, A. Denner. Comp. Phys. Commun., 64 (1991) *)
   drCO[ b___,DiracGamma[c_, di___],d:DiracGamma[_[__],___].. ,
         DiracGamma[c_,dim___],f___
       ] :=
        Block[ {drCOij, drCOld = Length[{d}]},
     (-1)^drCOld scev[c,c] ds[b,d,f]
     + 2 Sum[(-1)^(drCOij+1) coneins[ Pair[c,{d}[[drCOij,1]] ]
            * ds@@Join[{b},Drop[{d},{drCOij,drCOij}],{DiracGamma[c,dim],f}]
                                    ],{drCOij,1,drCOld}
            ]
              ]/;((Length[{d}]>0)&&FreeQ[c,LorentzIndex]&&
                 (!NumberQ[c]) && !MatchQ[{di}, {_Symbol -4}]);

(*g^mu g^i1 ... g^in g_mu, where g^mu and g_mu are in D dimensions,
  while g^ii (no g^5 here!) are in D, D-4 or 4 dimensions. This applies only for n>5
  The formula is given in Eq 2.9 of R.
  Mertig, M. Boehm, A. Denner. Comp. Phys. Commun., 64 (1991) *)
   drCO[ b___,DiracGamma[LorentzIndex[c_,di_Symbol],di_Symbol],
         d:DiracGamma[_[_,dim___],dim___].. ,
         DiracGamma[LorentzIndex[c_,di_Symbol],di_Symbol],f___
       ]:=
   Block[{idrCO,jdrCO,lddrCO = Length[{d}]},
        (-1)^lddrCO ( di - 2 lddrCO ) ds[b,d,f] -
          4 (-1)^lddrCO  Sum[ (-1)^(jdrCO-idrCO) *
         coneins[ Pair[{d}[[idrCO,1]],{d}[[jdrCO,1]] ] *
                  ds@@Join[ {b},Drop[ Drop[{d},{idrCO,idrCO}],
                                     {jdrCO-1,jdrCO-1}
                                    ],{f}
                          ]
                ],
                       {idrCO,1,lddrCO-1},{jdrCO,idrCO+1,lddrCO}
                            ]/.Pair->scev
         ] /;(Length[{d}]>5);

(* g^mu g^nu ...  g_mu  = - g^nu g^mu ... g_mu + 2 eta^mu~nu ... g_mu.
   This applies only if g^mu and g_mu are in different dimensions. *)
   drCO[ b___,DiracGamma[lv_[c_,dim___],dim___],
              DiracGamma[vl_[x__],dii___],d___,
              DiracGamma[lv_[c_,di___],di___],f___
       ]:=(-ds[b, DiracGamma[vl[x],dii],
                  DiracTrick[DiracGamma[lv[c,dim],dim],d,
                     DiracGamma[lv[c,di],di]], f
                ] + 2 coneins[Pair[vl[x], lv[c,dim]] *
                              ds[ b,d,DiracGamma[lv[c,di],di],f ]
                             ]
           ) /; {dim} =!= {di};



(* ************************************************************** *)
 SetAttributes[drS,Flat];
(* ************************************************************** *)
 SetAttributes[dr,Flat];   (* quite important!!! *)
(* ************************************************************** *)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracTrick | \n "]];
Null
