(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MemoryAvailable *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MemoryAvailable`",
             "HighEnergyPhysics`FeynCalc`"];

MemoryAvailable::"usage" =
"MemoryAvailable is an option of MemSet.
It can be set to an integer n,
where n is the available amount of main memory in Mega Byte.
The default setting is $MemoryAvailable.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MemoryAvailable | \n "]];
Null
