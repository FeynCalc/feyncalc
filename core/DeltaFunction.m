(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Dirac-delta function  (just a name) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DeltaFunction`",{"HighEnergyPhysics`FeynCalc`"}];

DeltaFunction::"usage"= "DeltaFunction is the Dirac delta-function.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(*Added 18 April 2001, Frederik Orellana*)
DeltaFunction[_?((NumericQ[#]===True&&(Positive[#]===True||Negative[#]===True))&)]:=0;
DeltaFunction[0]:=1;

DeltaFunction /:
   MakeBoxes[ DeltaFunction[y_], TraditionalForm] :=
    RowBox[{"\[Delta]", "(", Tbox[y], ")"}];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DeltaFunction | \n "]];
Null
