(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEj*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the m *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEj`",
             "HighEnergyPhysics`FeynCalc`"];

OPEj::usage= "OPEj is an dummy index in OPESum.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[PositiveInteger, DataType];
DataType[OPEj, PositiveInteger] = True;

   OPEj /: 
   MakeBoxes[OPEj ,TraditionalForm] := "j";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEj | \n "]];
Null
