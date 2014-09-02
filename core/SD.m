(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Kronecker delta for SU(N) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SD`",{"HighEnergyPhysics`FeynCalc`"}];

SD::"usage"=
"SD[i, j] is the (FeynCalc-external) Kronecker-delta for SU(N) with color
indices i and j. SD[i,j] is transformed into
SUNDelta[SUNIndex[i],SUNIndex[j]] by
FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

SetAttributes[SD, Orderless];

HighEnergyPhysics`FeynCalc`SD`SD /:
MakeBoxes[HighEnergyPhysics`FeynCalc`SD`SD[a_, b_], TraditionalForm] :=
    SubscriptBox["\[Delta]", HighEnergyPhysics`FeynCalc`Tbox[a,b]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SD | \n "]];
Null
