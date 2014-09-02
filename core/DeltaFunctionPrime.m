(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Dirac-delta function derivative (just a name) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DeltaFunctionPrime`",{"HighEnergyPhysics`FeynCalc`"}];

DeltaFunctionPrime::"usage"=
"DeltaFunctionPrime denotes the derivative of the Dirac delta-function.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeltaFunctionPrime /:
   MakeBoxes[ DeltaFunctionPrime[y_], TraditionalForm] :=
    RowBox[{SuperscriptBox["\[Delta]","\[Prime]"],
           "(", Tbox[y], ")"}
          ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DeltaFunctionPrime | \n "]];
Null
