(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: WriteOut is an option for several functions *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`WriteOut`",
             "HighEnergyPhysics`FeynCalc`"];

WriteOut::"usage" = 
"WriteOut is an option for OneLoop and SquareAmplitude. 
If set to True, the result of
OneLoop will be written to a file called \"name.res\", where name
is the first argument of OneLoop.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "WriteOut | \n "]];
Null
