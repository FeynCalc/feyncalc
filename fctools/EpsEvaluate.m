(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsEvaluate *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: simplification of Eps *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`fctools`EpsEvaluate`",
             "HighEnergyPhysics`FeynCalc`"];

EpsEvaluate::"usage" =
"EpsEvaluate[expr] applies total antisymmetry and
linearity (w.r.t. Momentum's) to all Levi-Civita tensors (Eps's) in expr .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Cases2, Eps , Expanding,
ExpandScalarProduct, FeynCalcInternal, LC, LCD, LorentzIndex, Momentum, Pair, PairContract,
LorentzIndex, ExplicitLorentzIndex];

EpsEvaluate[x_] := x /; FreeQ2[x,{LeviCivita, LC,LCD,Eps}];     (*EpsEvaluatedef*)
EpsEvaluate[ix_] := Block[{x = ix, nx,cx, tx, rud},
 x = FeynCalcInternal[ix];
 If[LeafCount[x] < 1000,    x//.Eps->Epsev,
    cx = Cases2[x, Eps];
    tx = Dispatch[Thread[rud[cx, cx//.Eps->Epsev]] /. rud->RuleDelayed];
    x/.tx
   ]                    ];


Epsev[A__] := ( Expand /@ (Distribute[DOT[A]]//
                    ExpandScalarProduct) )/.
              DOT->Epsevlin/.Epsevlin->Epsevantilin;
Epsevlin[a___,b_ c_Momentum,d___] := b Epsevlin[a,c,d];
Epsevlin[a___,b_ c_LorentzIndex,d___] := b Epsevlin[a,c,d];
(*Added 21/9-2002. F.Orellana. We're assuming that the Lorentz indices run from 1 to 4,
  not 0 to 3...*)
Epsevantilin[a:(_Integer|(LorentzIndex|ExplicitLorentzIndex)[_Integer]),
             b:(_Integer|(LorentzIndex|ExplicitLorentzIndex)[_Integer]),
             c:(_Integer|(LorentzIndex|ExplicitLorentzIndex)[_Integer]),
             d_] := Signature[{a,b,c,d} /. d :> (dd = Complement[{1,2,3,4},{a,b,c} /.
                   (LorentzIndex|ExplicitLorentzIndex)[x__]:>{x}[[1]]][[1]]) /.
                   (LorentzIndex|ExplicitLorentzIndex)[x__]:>{x}[[1]]]*
                   Pair[LorentzIndex[d], LorentzIndex[dd]];
(**)
Epsevantilin[a__] := Signature[{a}] Eps@@Sort[{a}];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsEvaluate | \n "]];
Null
