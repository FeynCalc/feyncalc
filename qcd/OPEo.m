(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEo*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the m *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEo`",
             "HighEnergyPhysics`FeynCalc`"];

OPEo::usage= "OPEo is n.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[PositiveInteger, DataType];
DataType[OPEo, PositiveInteger] = True;

   OPEo /:
   MakeBoxes[OPEo ,TraditionalForm] := "o";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEo | \n "]];
Null
