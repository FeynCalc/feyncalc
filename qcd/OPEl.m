(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEl*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  some dummy index*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEl`",
             "HighEnergyPhysics`FeynCalc`"];

OPEl::"usage"= "OPEl is an dummy index in OPESum.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[PositiveInteger, DataType];
DataType[OPEl, PositiveInteger] = True;
   OPEl /: 
   MakeBoxes[OPEl ,TraditionalForm] := "l";


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEl | \n "]];
Null
