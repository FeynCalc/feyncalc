(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Contraction of Eps'es *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`EpsContract`",{"HighEnergyPhysics`FeynCalc`"}];

EpsContract::"usage"=
"EpsContract is an option of Contract specifying whether Levi-Civita
tensors Eps[...] will be contracted, i.e., products
of two Eps are replaced via the determinant formula.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsContract | \n "]];
Null
