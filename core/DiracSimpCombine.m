(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracSimpCombine`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

DiracSimpCombine::"usage"=
"DiracSimpCombine is an option for DiracSimplify. If set to \
True, sums of DiracGamma's will be merged as much as \
possible in DiracGamma[ .. + .. + ]'s.";

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSimpCombine | \n "]];
Null
