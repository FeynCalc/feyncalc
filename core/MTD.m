(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MTD`",
             "HighEnergyPhysics`FeynCalc`"];

MTD::"usage"=
"MTD[mu, nu] is the metric tensor in D dimensions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci := fci = MakeContext["FeynCalcInternal"];

MTD /:
   MakeBoxes[ MTD[x_,y_], TraditionalForm ] :=
SuperscriptBox["g", HighEnergyPhysics`FeynCalc`Tbox[x,y]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MTD | \n "]];
Null
