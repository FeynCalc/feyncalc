(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  head for gauge-parameters *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`GaugeXi`",
             "HighEnergyPhysics`FeynCalc`"];

GaugeXi::"usage"= "GaugeXi is a head for gauge parameters.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   GaugeXi /:
   MakeBoxes[GaugeXi[a_], TraditionalForm] :=
    SubscriptBox["\[Xi]", TBox[a]];
   GaugeXi /:
   MakeBoxes[GaugeXi, TraditionalForm] :=
    TagBox["\[Xi]", TraditionalForm]

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GaugeXi | \n "]];
Null
