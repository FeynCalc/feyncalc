(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Vectors*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Vectors`",
             "HighEnergyPhysics`FeynCalc`"];

Vectors::usage= "Vectors is an option for FORM2FeynCalc. Its default
setting is Automatic. It may be set to a list, if the FORM-file does
not contain a V(ectors) statement.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Vectors, ReadProtected];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Vectors | \n "]];
Null
