(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDScalemu*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the s_n for  OPEInt*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`QCDScalemu`",
             "HighEnergyPhysics`FeynCalc`"];

QCDScalemu::usage= "QCDScalemu is ...";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

   QCDScalemu /:
   MakeBoxes[QCDScalemu, TraditionalForm] := \[Mu];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QCDScalemu | \n "]];
Null
