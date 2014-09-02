(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`LCD`",{"HighEnergyPhysics`FeynCalc`"}];

LCD::"usage"=
"LCD[m,n,r,s] evaluates to LeviCivita[m,n,r,s,Dimension->D]
applying FeynCalcInternal.
LCD[m,...][p, ...] evaluates to
LeviCivita[m,...,Dimension->D][p,...,Dimension->D]
applying FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   LCD /:
   MakeBoxes[LCD [x___][y___] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x,y]];
   LCD /:
   MakeBoxes[LCD [x__] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LCD | \n "]];
Null
