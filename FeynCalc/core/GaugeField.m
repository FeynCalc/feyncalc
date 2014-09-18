(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Gauge field *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`GaugeField`",{"HighEnergyPhysics`FeynCalc`"}];

GaugeField::"usage" =
"GaugeField is a name of a gauge field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   GaugeField /: MakeBoxes[GaugeField, TraditionalForm] := "A";

End[]; EndPackage[];
If[$VeryVerbose > 0,WriteString["stdout", "GaugeField | \n "]];
Null
