(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEn*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the m *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEn`",
             "HighEnergyPhysics`FeynCalc`"];

OPEn::usage= "OPEn is n.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[OPEn, ReadProtected];

MakeContext[PositiveInteger, DataType];
DataType[OPEn, PositiveInteger] = True;

   OPEn /:
   MakeBoxes[OPEn ,TraditionalForm] := "n";


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEn | \n "]];
Null
