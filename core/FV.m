(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FV`",
             "HighEnergyPhysics`FeynCalc`"];

FV::"usage"= "FV[p,mu] is a fourvector and is transformed into
Pair[Momentum[p], LorentzIndex[mu]]
by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Momentum SP, SPD];

FV[p_ /; Head[p]=!=Momentum, Momentum[b_]]:= SP[p,b];
FV[Momentum[p_], Momentum[b_]]:= SP[p,b];

   FV /: MakeBoxes[FV[a_Subscripted, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1,0]]], Tbox@@a[[1]], Tbox[b]];

   FV /: MakeBoxes[FV[a_Subscript, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1]]], Tbox@@Rest[a], Tbox[b]];

   FV /: MakeBoxes[FV[a_, b_], TraditionalForm] :=
            SuperscriptBox[Tbox[a], Tbox[b]];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FV | \n "]];
Null
