(* :Summary: Dimension is an option for several functions *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Dimension`",
               "HighEnergyPhysics`FeynCalc`"];

Dimension::"usage" =
"Dimension is an option for DiracMatrix, DiracSlash, FourVector, \
LeviCivita, MetricTensor, SetMandelstam, OneLoop and ScalarProduct. \
The default setting is sometimes 4, sometimes D. \
The setting should always be 4, a symbol (D, n, ...), or \
(D-4), (n-4), ... .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Dimension | \n "]];
Null
