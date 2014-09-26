(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairContract *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`PairContract`",{"HighEnergyPhysics`FeynCalc`"}];

PairContract::"usage" =
"PairContract is like Pair, but with (local) contraction properties.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

LorentzIndex:= LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
eps := eps                  = MakeContext["CoreObjects","Eps"];
factor2 := factor2          = MakeContext["Factor2"];
factoring := factoring      = MakeContext["CoreOptions","Factoring"];
freeq2 := freeq2            = MakeContext["FreeQ2"];
memset := memeset           = MakeContext["MemSet"];
MakeContext[MomentumExpand];

Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];

(* this option is only to by set by SetOptions ... *)
Options[PairContract] = {factoring -> False};

sCO = PairContract;

SetAttributes@@{{sCO,sceins,scev,sce,scevdoit,sczwei} ,Orderless};

scev[x_,y_]:= memset[ scev[x,y], scevdoit[x,y] ];
scev[x_,y_]:= scevdoit[x,y];
scevdoit[x_,y_] := Distribute[ sceins@@
                              ( Expand[ MomentumExpand/@{x,y} ] )
                    ]/.sceins->sczwei/.sczwei->sCO/.sCO->Pair;

sCO[ LorentzIndex[a_,di___], epsmu_ LorentzIndex[mu_, dimen___] ]:=
( epsmu /. LorentzIndex[mu,dimen]->LorentzIndex[a,di] ) /;
(!freeq2[epsmu, {eps, LorentzIndex[mu, dimen]}]);

sCO[ Momentum[x_,___],Momentum[polarization[x_,___]]]:=0;
sCO[ Momentum[x_,___],Momentum[polarization[n_?NumberQ x_,___]]]:=0;
sCO[Momentum[pi_,___],Momentum[polarization[x_Plus, ki___]]]:=
 scev[Momentum[x+pi], Momentum[polarization[x, ki]]]/;
     ( pi + Last[x] )===0;
sCO[Momentum[pi_,___],Momentum[polarization[x_Plus, ki___]]]:=
 scev[Momentum[pi-x], Momentum[polarization[x, ki]]]/;
             ( pi - Last[x] )===0;

sCO[ LorentzIndex[x_], LorentzIndex[x_] ]  := 4;
(*new ...*)
sCO[ LorentzIndex[x_], LorentzIndex[x_,_Symbol] ]  := 4;

sCO[ LorentzIndex[x_,di_], LorentzIndex[x_,di_] ] := di;
PairContract /: HoldPattern[PairContract[lor_[z_,___],x_]]^2 :=
                (PairContract[x,x]) /; lor === LorentzIndex;

(* CHANGE 09/94 *)
PairContract[Momentum[x_,___],Momentum[Polarization[x_, ___],___]] := 0;
PairContract[Momentum[x_,___],Momentum[Polarization[n_?NumberQ x_, ___],___]
    ] := 0;
PairContract[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= contract[expandscalarproduct[Pair[
             Momentum[x+pi, dii], Momentum[Polarization[x, ki], dii]]]
                ] /; ( pi + Last[x] ) === 0;
PairContract[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= contract[expandscalarproduct[Pair[
             Momentum[pi-x,dii], Momentum[Polarization[x, ki],dii]]]
                ] /; ( pi - Last[x] ) === 0;
(* by convention ... *)
PairContract[Momentum[Polarization[x_,__],___],
     Momentum[Polarization[x_,__],___] ] := -1;
(* CHANGE 09/94 *)

(*
PairContract/: PairContract[LorentzIndex[z_,___],x_] *
               PairContract[LorentzIndex[z_,___],y_] :=
           PairContract[x,y];
*)

PairContract/: PairContract[LorentzIndex[z_,___],x_] f_[a__] :=
(f[a]/.LorentzIndex[z,___]->x)/;
 (!FreeQ[f[a],LorentzIndex[z,___]]);

sCO[Momentum[a_Symbol,b_Symbol]]:=Pair[Momentum[a],Momentum[b]];

PairContract/:
   DOT[A___, HoldPattern[PairContract[lor_[z_,___],x_]], B___,
                 m_. f_[a__], c___ ] :=
 DOT[A,B,(m f[a]/.LorentzIndex[z,___]->x),c]/;
    ((!FreeQ[f[a], LorentzIndex[z,___]]) && (lor === LorentzIndex));

PairContract/: DOT[A___, m_. f_[a__], B___,
                   HoldPattern[PairContract[lor_[z_,___],x_]], c___ ] :=
 DOT[A.(m f[a]/.LorentzIndex[z,___]->x),B,c]/;
   ((!FreeQ[f[a]//Hold,LorentzIndex[z,___]]) && (lor === LorentzIndex));
(* **************************************************************** *)
(* definitions for dimension = D-4                                  *)
(* **************************************************************** *)
 sCO[ _[_,_Symbol-4],_[_] ]:=0;
 sCO[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ] :=
                                            sCO[v[x,di-4],w[y,di-4] ];
 sCO[ w_[y_,di_Symbol],v_[x_] ] := sCO[ v[x], w[y] ];
 sCO[ v_[x_], w_[y_,di_Symbol] ] := sCO[ v[x], w[y] ];
 sceins[0,_]:=0;                               (*sceinsdef*)
 sceins[a_LorentzIndex b_, c_] := b sceins[a, c];
 sceins[a_Momentum b_, c_] := b sceins[a, c];
 sczwei[ _[_],_[_,_Symbol-4] ]:=0;             (*sczweidef*)
 sczwei[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ]:=
                            sczwei[v[x, di-4], w[y, di-4]];
 sczwei[ w_[y_,di_Symbol],v_[x_] ]:=sczwei[ v[x],w[y] ];
 sce[x_,y_] := memset[set[x, y],      (*scedef*)
               If[(factoring /. Options[PairContract]) === True,
               factor2[
               Distribute[sceins@@( Expand[ MomentumExpand/@{x,y} ])
                         ]/.sceins->sczwei/.sczwei->Pair
                     ],
                Distribute[sceins@@( Expand[ MomentumExpand/@{x,y} ])
                         ]/.sceins->sczwei/.sczwei->Pair
                 ]   ];
 sCO[x_,y_] := memset[ sCO[x,y],
                      Block[{sCOt=sce[x,y]},
                       If[ FreeQ[ sCOt, Pair ] ||
                            (Head[sCOt]=!=Plus)
                           , sCOt,Pair[x,y]
                         ] ] ]/;FreeQ[{x,y},LorentzIndex];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PairContract | \n "]];
Null
