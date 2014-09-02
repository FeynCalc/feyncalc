(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairContract3 *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`PairContract3`",{"HighEnergyPhysics`FeynCalc`"}];

PairContract3::"usage" =
"PairContract3 is like Pair, but with local contraction properties
among PairContract3's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ ExpandScalarProduct, FreeQ2, LorentzIndex, Momentum, Pair];
SetAttributes[PairContract3,Orderless];

PairContract3[LorentzIndex[z_,di___],
              LorentzIndex[z_,di___]]:= If[{di}==={},4,di];
PairContract3[Momentum[a__], Momentum[b__]]:=
ExpandScalarProduct[Momentum[a], Momentum[b]];

PairContract3 /:
PairContract3[LorentzIndex[z__],LorentzIndex[x__]]^2 :=
PairContract3[LorentzIndex[x],LorentzIndex[x]];

PairContract3 /:
PairContract3[LorentzIndex[z__],x_]^2 :=
ExpandScalarProduct[x,x];


PairContract3/:
PairContract3[LorentzIndex[z__],x_] *
PairContract3[LorentzIndex[z__],y_] :=
If[FreeQ[{x,y},LorentzIndex],
   ExpandScalarProduct[x,y],
   PairContract3[x,y]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PairContract3 | \n "]];
Null
