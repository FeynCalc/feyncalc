(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: WriteOut *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: WriteOut is an option for several functions *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`WriteOut`",
             "HighEnergyPhysics`FeynCalc`"];

WriteOut::usage = 
"WriteOut is an option for OneLoop and SquareAmplitude. 
If set to True, the result of
OneLoop will be written to a file called \"name.res\", where name
is the first argument of OneLoop.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[WriteOut, ReadProtected];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "WriteOut | \n "]];
Null
