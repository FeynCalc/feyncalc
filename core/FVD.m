(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FVD`",
             "HighEnergyPhysics`FeynCalc`"];

FVD::"usage"= "FVD[p,mu] is a fourvector and is
transformed into Pair[Momentum[p,D], LorentzIndex[mu,D]]
by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   FVD /: MakeBoxes[FVD[a_Subscripted, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1,0]]], Tbox@@a[[1]], Tbox[b]];

   FVD /: MakeBoxes[FVD[a_Subscript, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1]]], Tbox@@Rest[a], Tbox[b]];

   FVD /: MakeBoxes[FVD[a_, b_], TraditionalForm] :=
            SuperscriptBox[Tbox[a], Tbox[b]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FVD | \n "]];
Null
