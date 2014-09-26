(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The head for complex conjugated indices *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ComplexIndex`",{"HighEnergyPhysics`FeynCalc`"}];

ComplexIndex::"usage"=
"ComplexIndex is the head of a complex conjugate index.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

ComplexIndex[ComplexIndex[x_]] := x;

ComplexIndex /:
   MakeBoxes[ComplexIndex[x_] ,TraditionalForm] :=
   SuperscriptBox[Tbox[x], "*"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ComplexIndex | \n "]];
Null
