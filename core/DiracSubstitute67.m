(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac chirality projection *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracSubstitute67`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracSubstitute67::"usage"=
"DiracSubstitute67 is an option for DiracSimplify. If set to \
True the chirality-projectors DiracGamma[6] and DiracGamma[7] are \
substituted by their definitions.";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSubstitute67 | \n "]];
Null
