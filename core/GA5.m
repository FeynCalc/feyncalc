(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`GA5`",
             "HighEnergyPhysics`FeynCalc`"];

GA5::"usage"=
"GA5 is equivalent to DiracGamma[5] and denotes gamma5.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DiracGamma];

GA5 = DiracGamma[5];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GA5 | \n "]];
Null
