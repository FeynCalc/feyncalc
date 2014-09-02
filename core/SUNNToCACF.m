(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option of SUNf and SUNSimplify *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SUNNToCACF`",{"HighEnergyPhysics`FeynCalc`"}];

SUNNToCACF::"usage"= "SUNNToCACF is an option of SUNSimplify. If set to
True, the Casimir operator eigenvalues CA (=N) and CF (=(N^2-1)/(2 N))
are introduced.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNNToCACF | \n "]];
Null
