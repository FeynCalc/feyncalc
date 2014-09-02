(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: CA = the N of SU(N) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`CA`",{"HighEnergyPhysics`FeynCalc`"}];


CA::"usage"=
"CA is one of the Casimir operator eigenvalues of SU(N); CA = N";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

CA /:
   MakeBoxes[
             CA, TraditionalForm
            ] := SubscriptBox["C", "A"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CA | \n "]];
Null
