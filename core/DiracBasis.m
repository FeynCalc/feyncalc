(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DiracBasis is just a auxiliary head for Dirac structures
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracBasis`",{"HighEnergyPhysics`FeynCalc`"}];

DiracBasis::"usage" =
"DiracBasis[any] is a head which is wrapped around Dirac structures \
(and the 1) as a result of the function DiracReduce. \
Eventually you want to substitute DiracBasis by Identity (or \
set: DiracBasis[1] = S; DiracBasis[DiracMatrix[mu]] = P; etc.).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracBasis | \n "]];
Null
