(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FORM *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FORM`",
             "HighEnergyPhysics`FeynCalc`"];

FORM::"usage" =
"FORM is a bolean option telling FeynCalc whether or not to use FORM for
evaluation. If set to True a FORM file is generated and run from Mathematica
and the result read back in. Currently, only RHI has this option and it is
required to be on a UNIX system and have R. Hamberg's FORM-program installed
correctly.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FORM | \n "]];
Null
