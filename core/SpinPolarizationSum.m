(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option for several functions *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SpinPolarizationSum`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

SpinPolarizationSum::"usage"=
"SpinPolarizationSum is an option for SquareAmplitude and
FermionSpinSum. The set (pure) function acts on the usual spin sum.";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinPolarizationSum | \n "]];
Null
