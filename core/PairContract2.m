(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairContract2 *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`PairContract2`",
             "HighEnergyPhysics`FeynCalc`"];

PairContract2::"usage" =
"PairContract2 is like Pair, but with local contraction properties
among PairContract2's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ FreeQ2, LorentzIndex, Momentum, Pair];

SetAttributes[PairContract2,Orderless];

(*
PairContract2[Momentum[a__], Momentum[b__] ] :=
Pair[Momentum[a], Momentum[b]];
*)

Clear[PairContract2];
PairContract2/:
PairContract2[LorentzIndex[z_],x_] *
PairContract2[LorentzIndex[z_],y_] :=
If[FreeQ[{x,y},LorentzIndex], Pair[x,y],
PairContract2[x,y]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PairContract2 | \n "]];
Null
