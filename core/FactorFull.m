(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FactorFull`",
             "HighEnergyPhysics`FeynCalc`"];

FactorFull::"usage"=
"FactorFull is an option of Factor2 (default False).
If set to False, products like
(a-b) (a+b) will be replaced by (a^2-b^2).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FactorFull | \n "]];
Null
