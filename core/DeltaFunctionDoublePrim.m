(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Dirac-delta function double derivative (just a name) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DeltaFunctionDoublePrime`",
             "HighEnergyPhysics`FeynCalc`"];

DeltaFunctionDoublePrime::"usage"=
"DeltaFunctionDoublePrime denotes the second derivative of the \
Dirac delta-function.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeltaFunctionDoublePrime /:
   MakeBoxes[ DeltaFunctionDoublePrime[y_], TraditionalForm] :=
    RowBox[{SuperscriptBox["\[Delta]","\[DoublePrime]"],
           "(", Tbox[y], ")"}
          ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DeltaFunctionDoublePrime | \n "]];
Null
