(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ScalarProductExpand expands scalar products *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ScalarProductExpand`",
             "HighEnergyPhysics`FeynCalc`"];

ScalarProductExpand::"usage" =
"ScalarProductExpand[expr]  expands scalar products of sums of
momenta in expr.
ScalarProductExpand[a, b] expands ScalarProduct[a, b].
ScalarProductExpand is equivalent to ExpandScalarProduct.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

ScalarProductExpand = MakeContext["ExpandScalarProduct"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ScalarProductExpand | \n "]];
Null
